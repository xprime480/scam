#include "expr/EvalOps.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamException.hpp"
#include "WorkQueue.hpp"
#include "expr/ClassWorker.hpp"
#include "expr/ClosureWorker.hpp"
#include "expr/ConsWorker.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/InstanceCont.hpp"
#include "expr/MapWorker.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "input/DictOpsParser.hpp"
#include "input/FunctionDefParser.hpp"
#include "input/SingletonParser.hpp"
#include "input/SymbolPlusManyParser.hpp"
#include "prim/PrimWorker.hpp"
#include "prim/Primitive.hpp"
#include "util/ArgListHelper.hpp"

#include <sstream>

using namespace scam;
using namespace std;

void scam::eval(ScamValue value, Continuation * cont, Env * env)
{
    if ( isCons(value) ) {
        workQueueHelper<ConsWorker>(cont, env, CAR(value), CDR(value));
    }

    else if ( isSymbol(value) ) {
        ScamValue evaluated;
        const ScamSymbol * hack = dynamic_cast<ScamSymbol *>(value);

        if ( env->check(hack) ) {
            evaluated = env->get(hack);
        }
        else {
            evaluated =
                ExpressionFactory::makeError("Symbol ",
                                             STRVAL(value),
                                             " does not exist",
                                             " in the current environment");
        }

        cont->run(evaluated);
    }

    else if ( isNull(value) ) {
        static const string msg{ "The null type cannot be evaluated." };
        static ScamValue expr = ExpressionFactory::makeError(msg, false);
        cont->run(expr);
    }

    else {
        // default case
        ScamData * hack = const_cast<ScamData *>(value);
        cont->run(hack);
    }
}

void scam::apply(ScamValue value,
                 ScamValue args,
                 Continuation * cont,
                 Env * env)
{
    if ( isClass(value) ) {
        /** It is not meaningful to do argument validation here as the
         ** correct args are not apparant until the instance init method
         ** is found.
         **/
        const ScamClass * hack = dynamic_cast<ScamClass *>(value);
        workQueueHelper<ClassWorker>(hack, args, cont, env);
    }

    else if ( isClosure(value) ) {
        workQueueHelper<ClosureWorker>(CLOSUREDEF(value),
                                       CLOSUREENV(value),
                                       cont,
                                       args,
                                       env,
                                       MACROLIKE(value));
    }

    else if ( isContinuation(value) ) {
        SingletonParser * parser = getSingletonOfAnythingParser();
        const bool accepted = parser->accept(args);

        if ( accepted ) {
            ScamValue arg = const_cast<ScamValue>(parser->get());
            CONTINUATION(value)->run(arg);
        }
        else {
            failedArgParseMessage(writeValue(value).c_str(),
                                  "(form)",
                                  args,
                                  cont);
        }
    }

    else if ( isDict(value) ) {
        DictOpsParser * parser = standardMemoryManager.make<DictOpsParser>();

        if ( ! parser->accept(args) ) {
            failedArgParseMessage("dict", "(:op args{0,2})", args, cont);
            return;
        }

        const ScamKeyword * op = parser->getParsedOp();
        ScamValue rv = nullptr;

        auto opHack =  const_cast<ScamData *>(dynamic_cast<const ScamData *>(op));
        auto getHack = const_cast<ScamData *>(dynamic_cast<const ScamData *>(DictOpsParser::getOp));
        auto putHack = const_cast<ScamData *>(dynamic_cast<const ScamData *>(DictOpsParser::putOp));
        auto lenHack = const_cast<ScamData *>(dynamic_cast<const ScamData *>(DictOpsParser::lenOp));
        auto hasHack = const_cast<ScamData *>(dynamic_cast<const ScamData *>(DictOpsParser::hasOp));
        auto remHack = const_cast<ScamData *>(dynamic_cast<const ScamData *>(DictOpsParser::remOp));

        auto dictHack = dynamic_cast<ScamDict *>(value);

        if ( equals(opHack, getHack) ) {
            rv = dictHack->get(parser->getOpKey());
        }
        else if ( equals(opHack, putHack) ) {
            /* value is potentially UB so revisit value soon!! */
            ScamValue val = parser->getOpVal();
            rv = dictHack->put(parser->getOpKey(), val);
        }
        else if ( equals(opHack, lenHack) ) {
            rv = ExpressionFactory::makeInteger(length(value), true);
        }
        else if ( equals(opHack, hasHack) ) {
            const bool b = dictHack->has(parser->getOpKey());
            rv = ExpressionFactory::makeBoolean(b);
        }
        else if ( equals(opHack, remHack) ) {
            rv = dictHack->remove(parser->getOpKey());
        }
        else {
            rv = ExpressionFactory::makeError("Unknown dictionary operator: ",
                                              writeValue(op));
        }

        cont->run(rv);
    }

    else if ( isInstance(value) ) {
        InstanceParser * parser = standardMemoryManager.make<InstanceParser>();

        if ( ! parser->accept(args) ) {
            failedArgParseMessage("instance", "(sym forms*)", args, cont);
            return;
        }

        ScamEnvKeyType name = parser->getSymbol();
        ScamValue funargs = parser->getForms();

        Continuation * newCont =
            standardMemoryManager.make<InstanceCont>(value, name, cont);
        if ( isNil(funargs) ) {
            newCont->run(funargs);
        }
        else {
            mapEval(funargs, newCont, env);
        }
    }

    else if ( isSpecialForm(value) ) {
        SFFUNC(value)(args, cont, env, SFENGINE(value));
    }

    else if ( isPrimitive(value) ) {
        /*
         * For primitives, the argument confirmation is delegated to the
         * derived class' applyArgs function.
         */
        Primitive * hack = dynamic_cast<Primitive *>(value);
        workQueueHelper<PrimWorker>(cont, env, args, hack);
    }

    else {
        // default case
        ScamValue err =
            ExpressionFactory::makeError("Not possible to apply <",
                                         writeValue(value),
                                         "> to args ",
                                         writeValue(args));
        cont->run(err);
    }
}

void scam::mapEval(ScamValue value, Continuation * cont, Env * env)
{
    if ( isCons(value) ) {
        workQueueHelper<MapWorker>(cont, env, CAR(value), CDR(value));
    }

    else {
        // default case
        ScamData * hack = const_cast<ScamData *>(value);
        cont->run(hack);
    }
}

ScamValue scam::withEnvUpdate(ScamValue value, Env * updated)
{
    if ( ! isClosure(value) ) {
        stringstream s;
        s << "Cannot update env of <" << writeValue(value) << ">";
        throw ScamException(s.str());
    }

    return ExpressionFactory::makeClosure(CLOSUREDEF(value),
                                          updated,
                                          MACROLIKE(value));
}
