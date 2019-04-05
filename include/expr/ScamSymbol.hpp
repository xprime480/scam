#if ! defined(SCAMSYMBOL_H)
#define SCAMSYMBOL_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamSymbol : public ScamExpr
    {
    private:
        ScamSymbol(std::string const & value);

    public:
        static ScamSymbol * makeInstance(std::string const & value);

        std::string toString() const override;
        void eval(ContHandle cont, Env env) override;

        bool isSymbol() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        std::string const value;
    };
}

#endif
