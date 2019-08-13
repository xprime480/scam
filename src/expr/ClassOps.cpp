#include "expr/ClassOps.hpp"

#include "ErrorCategory.hpp"
#include "ScamException.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "util/ClassDef.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static ScamValue self = makeSymbol("self", false);
    static ScamValue parentKey = makeSymbol("parent", false);

    extern void checkClass(ScamValue cls, const char * op);
    extern void checkInstance(ScamValue inst, const char * op);
}

ScamValue scam::getClassBase(ScamValue cls)
{
    checkClass(cls, "getClassBase");
    return cls->classDef().base;
}

size_t scam::getClassVarCount(ScamValue cls)
{
    checkClass(cls, "getClassVarCount");
    return length(cls->classDef().vars);
}

ScamValue scam::getClassVar(ScamValue cls, size_t idx)
{
    checkClass(cls, "getClassVar");
    return nthcar(cls->classDef().vars, idx);
}

size_t scam::getClassMethodCount(ScamValue cls)
{
    checkClass(cls, "getClassMethodCount");
    return cls->classDef().methods.size();
}

const FunctionDef & scam::getClassMethod(ScamValue cls, size_t idx)
{
    checkClass(cls, "getClassMethod");
    return cls->classDef().methods[idx];
}

Env * scam::getClassCapture(ScamValue cls)
{
    checkClass(cls, "getClassCapture");
    return cls->classEnv();
}

Env * scam::getInstanceFunctionMap(ScamValue inst)
{
    checkInstance(inst, "getInstanceFunctionMap");
    return inst->instancePrivate();
}

Env * scam::getInstanceEnv(ScamValue inst)
{
    checkInstance(inst, "getInstanceEnv");
    return inst->instanceLocal();
}

ScamValue scam::getInstanceParent(ScamValue inst)
{
    checkInstance(inst, "getInstanceParent");
    Env *& local = inst->instanceLocal();

    ScamValue test = local->check(parentKey);
    if ( isUnhandledError(test) ) {
        return test;
    }
    else if ( truth(test) ) {
        return local->get(parentKey);
    }

    return makeNull();
}

ScamValue scam::setInstanceSelf(ScamValue inst, ScamValue expr)
{
    checkInstance(inst, "setInstanceSelf");
    return inst->instanceLocal()->put(self, expr);
}

ScamValue scam::setInstanceParent(ScamValue inst, ScamValue expr)
{
    checkInstance(inst, "setInstanceParent");
    return inst->instanceLocal()->put(parentKey, expr);
}

ScamValue scam::getInstanceMethod(ScamValue value, ScamValue name)
{
    while ( isInstance(value) ) {
        Env * env = getInstanceFunctionMap(value);
        ScamValue test = env->check(name);
        if ( isUnhandledError(test) ) {
            return test;
        }
        else if ( truth(test) ) {
            return env->get(name);
        }

        value = getInstanceParent(value);
        if ( isUnhandledError(value) ) {
            return value;
        }
    }

    ScamValue err = makeError("Instance method not found", name);
    err->errorCategory() = evalCategory;
    return err;
}

namespace
{
    void checkClass(ScamValue cls, const char * op)
    {
        if ( ! isClass(cls) ) {
            stringstream s;
            s << "cannot perform class operation " << op
              << " on non-class value<" << writeValue(cls) << ">";
            throw ScamException(s.str());
        }
    }

    void checkInstance(ScamValue inst, const char * op)
    {
        if ( ! isInstance(inst) ) {
            stringstream s;
            s << "cannot perform instance operation " << op
              << " on non-instance value<" << writeValue(inst) << ">";
            throw ScamException(s.str());
        }
    }
}
