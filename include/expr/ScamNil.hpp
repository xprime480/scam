#if ! defined(SCAMNIL_H)
#define SCAMNIL_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamNil : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamNil();
        static ScamNil * makeInstance();
    };
}

#endif
