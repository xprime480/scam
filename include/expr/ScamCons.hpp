#if ! defined(SCAMCONS_H)
#define SCAMCONS_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCons : public ScamExpr
    {
    public:
        ScamCons(ScamExpr * car, ScamExpr * cdr);

        std::string toString() const override;
        void eval(ContHandle cont, Env env) override;
        void mapEval(ContHandle cont, Env env) override;

        bool isCons() const override;
        bool isList() const override;

        ExprHandle getCar() const override;
        ExprHandle getCdr() const override;

        size_t length() const override;
        ExprHandle nthcar(size_t n) const override;
        ExprHandle nthcdr(size_t n) const override;

    private:
        ExprHandle car;
        ExprHandle cdr;
    };
}

#endif
