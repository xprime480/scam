#include "expr/EvalOps.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/ClassOps.hpp"
#include "expr/ClassWorker.hpp"
#include "expr/ClosureWorker.hpp"
#include "expr/ConsWorker.hpp"
#include "expr/EqualityOps.hpp"
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

void scam::eval(ScamValue value, Continuation * cont, Env * env)
{
    if ( isPair(value) ) {
        workQueueHelper<ConsWorker>(value->carValue(),
                                    value->cdrValue(),
                                    cont,
                                    env);
    }

    else if ( isSymbol(value) ) {
        ScamValue test = env->check(value);
        if ( isError(test) ) {
            ScamEngine::getEngine().handleError(test);
        }
        else if ( truth(test) ) {
            ScamValue evaluated = env->get(value);
            cont->handleValue(evaluated);
        }
        else {
            ScamValue err = makeError("Symbol not found (%{0})", value);
            err->errorCategory() = evalCategory;
            ScamEngine::getEngine().handleError(err);
        }
    }

    else if ( isNothing(value) ) {
        static const char * msg{ "The null type cannot be evaluated." };
        ScamValue err = makeError(msg);
        err->errorCategory() = evalCategory;
        ScamEngine::getEngine().handleError(err);
    }

    else {
        // default case
        cont->handleValue(value);
    }
}

void
scam::apply(ScamValue value, ScamValue args, Continuation * cont, Env * env)
{
    static const char * name { "apply" };
    if ( isClass(value) ) {
        workQueueHelper<ClassWorker>(value, args, cont, env);
    }

    else if ( isClosure(value) ) {
        workQueueHelper<ClosureWorker>(value, cont, args, env);
    }

    else if ( isContinuation(value) ) {
        ObjectParameter p0;
        if ( argsToParms(args, name, p0) ) {
            ScamValue arg = p0.value;
            eval(arg, value->contValue(), env);
        }
    }

    else if ( isDict(value) ) {
        DictCommand cmd;
        if ( ! argsToParms(args, name, cmd) ) {
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
            rv = dictHas(value, cmd.key);
        }
        else if ( equals(op, DictCommand::remOp) ) {
            rv = dictRemove(value, cmd.key);
        }
        else {
            rv = makeError("Unknown dictionary operator", op);
            rv->errorCategory() = evalCategory;
        }

        if ( isUnhandledError(rv) ) {
            ScamEngine::getEngine().handleError(rv);
        }
        else {
            cont->handleValue(rv);
        }
    }

    else if ( isInstance(value) ) {
        InstanceDef def;
        if ( argsToParms(args, name, def) ) {
            ScamValue name    = def.name;
            ScamValue funargs = def.forms;

            ScamValue method = getInstanceMethod(value, name);
            if ( isUnhandledError(method) ) {
                ScamEngine::getEngine().handleError(method);
            }
            else {
                apply(method, funargs, cont, env);
            }
        }
    }

    else if ( isSpecialForm(value) ) {
        (value->sfFunc())(args, cont, env);
    }

    else if ( isPrimitive(value) ) {
        workQueueHelper<PrimWorker>(value, args, cont, env);
    }

    else if ( isSyntax(value) ) {
        value->syntaxRules().applySyntax(args, cont, env);
    }

    else {
        // default case
        ScamValue err = makeError("Cannot apply %{0} to %{1}", value, args);
        err->errorCategory() = evalCategory;
        ScamEngine::getEngine().handleError(err);
    }
}

Env * scam::env(ScamValue value)
{
    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    return mm.make<Env>();
}

void scam::mapEval(ScamValue value, Continuation * cont, Env * env)
{
    if ( isPair(value) ) {
        workQueueHelper<MapWorker>(value->carValue(),
                                   value->cdrValue(),
                                   cont,
                                   env);
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
