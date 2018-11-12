#if ! defined(SCAMCLASS_H)
#define SCAMCLASS_H 1

#include "Env.hpp"

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamClass : public ScamExpr
    {
    public:
        ScamClass(ScamExpr * base, ScamExpr * vars, ScamExpr * funs);

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ScamExpr * args, ContHandle cont, Env env) override;

        bool isProcedure() const override;
        bool isClass() const override;

    private:
        ExprHandle base;
        ExprHandle vars;
        ExprHandle funs;
    };
}

#endif
