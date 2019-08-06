#if ! defined(CLASSOPS_HPP)
#define CLASSOPS_HPP 1

#include "ScamFwd.hpp"

#include <cstddef>

namespace scam
{
    class FunctionDef;

    extern ScamValue getClassBase(ScamValue cls);
    extern size_t getClassVarCount(ScamValue cls);
    extern ScamValue getClassVar(ScamValue cls, size_t idx);
    extern size_t getClassMethodCount(ScamValue cls);
    extern const FunctionDef & getClassMethod(ScamValue cls, size_t idx);
    extern Env * getClassCapture(ScamValue cls);

    extern Env * getInstanceFunctionMap(ScamValue inst);
    extern Env * getInstanceEnv(ScamValue inst);
    extern ScamValue getInstanceParent(ScamValue inst);
    extern ScamValue setInstanceSelf(ScamValue inst, ScamValue expr);
    extern ScamValue setInstanceParent(ScamValue inst, ScamValue expr);
}

#endif
