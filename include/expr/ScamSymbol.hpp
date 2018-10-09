#if ! defined(SCAMSYMBOL_H)
#define SCAMSYMBOL_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamSymbol : public ScamExpr
    {
    public:
        ScamSymbol(std::string const & value);
        std::string toString() const override;
        void eval(ContHandle cont, Env & env) override;

        bool isSymbol() const override;

        ExprHandle clone() const override;

    private:
        std::string const value;
    };
}

#endif
