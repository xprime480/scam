#if ! defined(SCAMERROR_H)
#define SCAMERROR_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class ScamError : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamError(char const * msg, bool managed = true);
        static ScamError * makeInstance(char const * msg, bool managed = true);
    };
}

#endif
