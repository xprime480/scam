#if ! defined(SCAMCHARACTER_H)
#define SCAMCHARACTER_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCharacter : public ScamExpr
    {
    private:
        ScamCharacter(std::string const & value);

    public:
        static ScamCharacter * makeInstance(std::string const & value);

        std::string toString() const override;

        bool isChar() const override;
        char toChar() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        std::string const value;
    };
}

#endif
