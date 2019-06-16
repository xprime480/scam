#include "expr/EvalOps.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "WorkQueue.hpp"
#include "expr/ClassWorker.hpp"
#include "expr/ClosureWorker.hpp"
#include "expr/ConsWorker.hpp"
#include "expr/InstanceCont.hpp"
#include "expr/MapWorker.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "input/DictOpsParser.hpp"
#include "input/FunctionDefParser.hpp"
#include "input/SingletonParser.hpp"
#include "input/SymbolPlusManyParser.hpp"
#include "prim/PrimWorker.hpp"
#include "util/ArgListHelper.hpp"

#include <sstream>

using namespace scam;
using namespace std;

void scam::eval(ScamValue value,
                Continuation * cont,
                Env * env,
                ScamEngine * engine)
{
    if ( isPair(value) ) {
        workQueueHelper<ConsWorker>(cont, env, CAR(value), CDR(value), engine);
    }

    else if ( isSymbol(value) ) {
        if ( env->check(value) ) {
            ScamValue evaluated = env->get(value);
            cont->handleValue(evaluated);
        }
        else {
            ScamValue err = makeErrorExtended("Symbol ",
                                              writeValue(value),
                                              " does not exist",
                                              " in the current environment");
            engine->handleError(err);
        }
    }

    else if ( isNothing(value) ) {
        static const string msg{ "The null type cannot be evaluated." };
        static ScamValue err = makeError(msg, false);
        engine->handleError(err);
    }

    else {
        // default case
        cont->handleValue(value);
    }
}

void scam::apply(ScamValue value,
                 ScamValue args,
                 Continuation * cont,
                 Env * env,
                 ScamEngine * engine)
{
    if ( isClass(value) ) {
        workQueueHelper<ClassWorker>(value, args, cont, env, engine);
    }

    else if ( isClosure(value) ) {
        workQueueHelper<ClosureWorker>(CLOSUREDEF(value),
                                       CLOSUREENV(value),
                                       cont,
                                       args,
                                       env,
                                       MACROLIKE(value),
                                       engine);
    }

    else if ( isContinuation(value) ) {
        SingletonParser * parser = getSingletonOfAnythingParser();
        const bool accepted = parser->accept(args);

        if ( accepted ) {
            ScamValue arg = parser->get();
            CONTINUATION(value)->handleValue(arg);
        }
        else {
            failedArgParseMessage(writeValue(value).c_str(),
                                  "(form)",
                                  args,
                                  cont,
                                  engine);
        }
    }

    else if ( isDict(value) ) {
        DictOpsParser * parser = standardMemoryManager.make<DictOpsParser>();

        if ( ! parser->accept(args) ) {
            failedArgParseMessage("dict",
                                  "(:op args{0,2})",
                                  args,
                                  cont,
                                  engine);
            return;
        }

        ScamValue op = parser->getParsedOp();
        ScamValue rv = nullptr;

        if ( equals(op, DictOpsParser::getOp) ) {
            rv = dictGet(value, parser->getOpKey());
        }
        else if ( equals(op, DictOpsParser::putOp) ) {
            ScamValue val = parser->getOpVal();
            rv = dictPut(value, parser->getOpKey(), val);
        }
        else if ( equals(op, DictOpsParser::lenOp) ) {
            rv = makeInteger(length(value), true);
        }
        else if ( equals(op, DictOpsParser::hasOp) ) {
            const bool b = dictHas(value, parser->getOpKey());
            rv = makeBoolean(b);
        }
        else if ( equals(op, DictOpsParser::remOp) ) {
            rv = dictRemove(value, parser->getOpKey());
        }
        else {
            rv = makeErrorExtended("Unknown dictionary operator: ",
                                   writeValue(op));
            engine->handleError(rv);
            return;
        }

        cont->handleValue(rv);
    }

    else if ( isInstance(value) ) {
        InstanceParser * parser = standardMemoryManager.make<InstanceParser>();

        if ( ! parser->accept(args) ) {
            failedArgParseMessage("instance",
                                  "(sym forms*)",
                                  args,
                                  cont,
                                  engine);
            return;
        }

        ScamValue name = parser->getSymbol();
        ScamValue funargs = parser->getForms();

        Continuation * newCont =
            standardMemoryManager.make<InstanceCont>(value, name, cont, engine);
        if ( isNull(funargs) ) {
            newCont->handleValue(funargs);
        }
        else {
            mapEval(funargs, newCont, env, engine);
        }
    }

    else if ( isSpecialForm(value) ) {
        SFFUNC(value)(args, cont, env, SFENGINE(value));
    }

    else if ( isPrimitive(value) ) {
        workQueueHelper<PrimWorker>(cont, env, engine, args, value);
    }

    else {
        // default case
        ScamValue err = makeErrorExtended("Not possible to apply <",
                                          writeValue(value),
                                          "> to args ",
                                          writeValue(args));
        engine->handleError(err);
    }
}

void scam::mapEval(ScamValue value,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine)
{
    if ( isPair(value) ) {
        workQueueHelper<MapWorker>(cont, env, CAR(value), CDR(value), engine);
    }

    else {
        // default case
        cont->handleValue(value);
    }
}

ScamValue scam::withEnvUpdate(ScamValue value, Env * updated)
{
    if ( ! isClosure(value) ) {
        stringstream s;
        s << "Cannot update env of <" << writeValue(value) << ">";
        throw ScamException(s.str());
    }

    return makeClosure(CLOSUREDEF(value), updated, MACROLIKE(value));
}
