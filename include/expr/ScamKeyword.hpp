#if ! defined(SCAMKEYWORD_H)
#define SCAMKEYWORD_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamKeyword : public ScamExpr
    {
    private:
        ScamKeyword(std::string const & value);

    public:
        static ScamKeyword * makeInstance(std::string const & value);

        std::string toString() const override;

        bool isKeyword() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        std::string const value;
    };
}

#endif
