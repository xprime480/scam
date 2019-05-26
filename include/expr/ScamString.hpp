#if ! defined(SCAMSTRING_H)
#define SCAMSTRING_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamString : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamString(std::string const & value);
        static ScamString * makeInstance(std::string const & value);
    };
}

#endif
