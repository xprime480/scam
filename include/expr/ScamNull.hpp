#if ! defined(SCAMNULL_H)
#define SCAMNULL_H 1

#include "expr/ScamExpr.hpp"
#include "expr/ScamData.hpp"

#include <string>

namespace scam
{
    class ScamNull : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamNull();
        static ScamNull * makeInstance();
    };
}

#endif
