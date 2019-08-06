#include "expr/ClassOps.hpp"

#include "Env.hpp"
#include "ScamException.hpp"
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

namespace
{
    extern void checkClass(ScamValue cls, const char * op)
    {
        if ( ! isClass(cls) ) {
            stringstream s;
            s << "cannot perform class operation " << op
              << " on non-class value<" << writeValue(cls) << ">";
            throw ScamException(s.str());
        }
    }

    extern void checkInstance(ScamValue inst, const char * op)
    {
        if ( ! isInstance(inst) ) {
            stringstream s;
            s << "cannot perform instance operation " << op
              << " on non-instance value<" << writeValue(inst) << ">";
            throw ScamException(s.str());
        }
    }
}
