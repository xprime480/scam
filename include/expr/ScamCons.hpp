#if ! defined(SCAMCONS_H)
#define SCAMCONS_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCons : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamCons(ScamValue car, ScamValue cdr);
        static ScamCons * makeInstance(ScamValue car, ScamValue cdr);
    };
}

#endif
