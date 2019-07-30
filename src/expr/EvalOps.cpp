#include "expr/EvalOps.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "WorkQueue.hpp"
#include "expr/ClassWorker.hpp"
#include "expr/ClosureWorker.hpp"
#include "expr/ConsWorker.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/InstanceCont.hpp"
#include "expr/MapWorker.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "form/SyntaxRules.hpp"
#include "prim/PrimWorker.hpp"
#include "util/ArgListHelper.hpp"
#include "util/DictCommand.hpp"
#include "util/InstanceDef.hpp"

#include <sstream>

using namespace scam;
using namespace std;

void scam::eval(ScamValue value,
                Continuation * cont,
                Env * env,
                ScamEngine * engine)
{
    if ( isPair(value) ) {
        workQueueHelper<ConsWorker>(value->carValue(),
                                    value->cdrValue(),
                                    cont,
                                    env,
                                    engine);
    }

    else if ( isSymbol(value) ) {
        if ( env->check(value) ) {
            ScamValue evaluated = env->get(value);
            cont->handleValue(evaluated);
        }
        else {
            ScamValue err = makeError("Symbol not found (%{0})", value);
            engine->handleError(err);
        }
    }

    else if ( isNothing(value) ) {
        static const char * msg{ "The null type cannot be evaluated." };
        ScamValue err = makeError(msg);
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
    static const char * name { "apply" };
    if ( isClass(value) ) {
        workQueueHelper<ClassWorker>(value, args, cont, env, engine);
    }

    else if ( isClosure(value) ) {
        workQueueHelper<ClosureWorker>(value->closureDef(),
                                       value->closureEnv(),
                                       cont,
                                       args,
                                       env,
                                       engine);
    }

    else if ( isContinuation(value) ) {
        ObjectParameter p0;
        if ( argsToParms(args, engine, name, p0) ) {
            ScamValue arg = p0.value;
            eval(arg, value->contValue(), env, engine);
        }
    }

    else if ( isDict(value) ) {
        DictCommand cmd;
        if ( ! argsToParms(args, engine, name, cmd) ) {
            return;
        }

        ScamValue op = cmd.op;
        ScamValue rv = nullptr;

        if ( equals(op, DictCommand::getOp) ) {
            rv = dictGet(value, cmd.key);
        }
        else if ( equals(op, DictCommand::putOp) ) {
            ScamValue val = cmd.val;
            rv = dictPut(value, cmd.key, val);
        }
        else if ( equals(op, DictCommand::lenOp) ) {
            rv = makeInteger(length(value), true);
        }
        else if ( equals(op, DictCommand::hasOp) ) {
            const bool b = dictHas(value, cmd.key);
            rv = makeBoolean(b);
        }
        else if ( equals(op, DictCommand::remOp) ) {
            rv = dictRemove(value, cmd.key);
        }
        else {
            rv = makeError("Unknown dictionary operator", op);
            engine->handleError(rv);
            return;
        }

        cont->handleValue(rv);
    }

    else if ( isInstance(value) ) {
        InstanceDef def;
        if ( argsToParms(args, engine, name, def) ) {
            ScamValue name    = def.name;
            ScamValue funargs = def.forms;

            Continuation * newCont =
                standardMemoryManager.make<InstanceCont>(value,
                                                         name,
                                                         cont,
                                                         engine);
            if ( isNull(funargs) ) {
                newCont->handleValue(funargs);
            }
            else {
                mapEval(funargs, newCont, env, engine);
            }
        }
    }

    else if ( isSpecialForm(value) ) {
        (value->sfFunc())(args, cont, env, value->sfEngine());
    }

    else if ( isPrimitive(value) ) {
        workQueueHelper<PrimWorker>(value, args, cont, env, engine);
    }

    else if ( isSyntax(value) ) {
        value->syntaxRules().applySyntax(args, cont, env, engine);
    }

    else {
        // default case
        ScamValue err = makeError("Cannot apply", value, args);
        engine->handleError(err);
    }
}

Env * scam::env(ScamValue value)
{
    return standardMemoryManager.make<Env>();
}

void scam::mapEval(ScamValue value,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine)
{
    if ( isPair(value) ) {
        workQueueHelper<MapWorker>(value->carValue(),
                                   value->cdrValue(),
                                   cont,
                                   env,
                                   engine);
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

    return makeClosure(value->closureDef(), updated);
}
