#if ! defined(SCAMCLASS_H)
#define SCAMCLASS_H 1

#include "Env.hpp"

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamClassAdapter;

    class ScamClass : public ScamExpr
    {
        ScamClass(ScamExpr * base,
                  ScamExpr * vars,
                  ScamExpr * funs,
                  Env capture);

    public:
        static ScamClass * makeInstance(ScamExpr * base,
                                        ScamExpr * vars,
                                        ScamExpr * funs,
                                        Env capture);

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ScamExpr * args, ContHandle cont, Env env) override;

        bool isProcedure() const override;
        bool isClass() const override;

        friend class ScamClassAdapter;

    private:
        ScamExpr * base;
        ScamExpr * vars;
        ScamExpr * funs;
        Env        capture;
    };
}

#endif
