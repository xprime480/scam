#if ! defined(SCAMCHARACTER_H)
#define SCAMCHARACTER_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCharacter : public ScamExpr
    {
    public:
        ScamCharacter(std::string const & value);
        std::string toString() const override;

        bool isChar() const override;
        char toChar() const override;

        ExprHandle clone() const override;

    private:
        std::string const value;
    };
}

#endif
