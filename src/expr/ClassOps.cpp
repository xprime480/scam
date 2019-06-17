#include "expr/ClassOps.hpp"

#include "Env.hpp"
#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "input/ClassDefParser.hpp"

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
    return cls->classDef()->getBase();
}

size_t scam::getClassVarCount(ScamValue cls)
{
    checkClass(cls, "getClassVarCount");
    return cls->classDef()->getVarCount();
}

ScamValue scam::getClassVar(ScamValue cls, size_t idx)
{
    checkClass(cls, "getClassVar");
    return cls->classDef()->getVar(idx);
}

size_t scam::getClassMethodCount(ScamValue cls)
{
    checkClass(cls, "getClassMethodCount");
    return cls->classDef()->getMethodCount();
}

const FunctionDefParser * scam::getClassMethod(ScamValue cls, size_t idx)
{
    checkClass(cls, "getClassMethod");
    return cls->classDef()->getMethod(idx);
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

    if ( local->check(parentKey) ) {
        return local->get(parentKey);
    }

    return makeNull();
}

void scam::setInstanceSelf(ScamValue inst, ScamValue expr)
{
    checkInstance(inst, "setInstanceSelf");
    inst->instanceLocal()->put(self, expr);
}

void scam::setInstanceParent(ScamValue inst, ScamValue expr)
{
    checkInstance(inst, "setInstanceParent");
    inst->instanceLocal()->put(parentKey, expr);
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
