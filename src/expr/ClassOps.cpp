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
    return CLASSDEF(cls)->getBase();
}

size_t scam::getClassVarCount(ScamValue cls)
{
    checkClass(cls, "getClassVarCount");
    return CLASSDEF(cls)->getVarCount();
}

ScamValue scam::getClassVar(ScamValue cls, size_t idx)
{
    checkClass(cls, "getClassVar");
    return CLASSDEF(cls)->getVar(idx);
}

size_t scam::getClassMethodCount(ScamValue cls)
{
    checkClass(cls, "getClassMethodCount");
    return CLASSDEF(cls)->getMethodCount();
}

const FunctionDefParser * scam::getClassMethod(ScamValue cls, size_t idx)
{
    checkClass(cls, "getClassMethod");
    return CLASSDEF(cls)->getMethod(idx);
}

Env * scam::getClassCapture(ScamValue cls)
{
    checkClass(cls, "getClassCapture");
    return CLASSENV(cls);
}

Env * scam::getInstanceFunctionMap(ScamValue inst)
{
    checkInstance(inst, "getInstanceFunctionMap");
    return INSTANCEPRIVENV(inst);
}

Env * scam::getInstanceEnv(ScamValue inst)
{
    checkInstance(inst, "getInstanceEnv");
    return INSTANCELOCALENV(inst);
}

ScamValue scam::getInstanceParent(ScamValue inst)
{
    checkInstance(inst, "getInstanceParent");

    if ( INSTANCELOCALENV(inst)->check(parentKey) ) {
        return INSTANCELOCALENV(inst)->get(parentKey);
    }

    return makeNil();
}

void scam::setInstanceSelf(ScamValue inst, ScamValue expr)
{
    checkInstance(inst, "setInstanceSelf");
    INSTANCELOCALENV(inst)->put(self, expr);
}

void scam::setInstanceParent(ScamValue inst, ScamValue expr)
{
    checkInstance(inst, "setInstanceParent");
    INSTANCELOCALENV(inst)->put(parentKey, expr);
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
