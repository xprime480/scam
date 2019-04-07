#if ! defined(SCAMKEYWORD_H)
#define SCAMKEYWORD_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamKeyword : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamKeyword(std::string const & value);
        static ScamKeyword * makeInstance(std::string const & value);

    public:
        std::string toString() const override;

        bool isKeyword() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        std::string const value;
    };
}

#endif
