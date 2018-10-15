#if ! defined(SCAMCLOSURE_H)
#define SCAMCLOSURE_H 1

#include "Env.hpp"

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamClosure : public ScamExpr
    {
    public:
        ScamClosure(ExprHandle const & args,
                    ExprHandle const & forms,
                    Env & env);

        std::string toString() const override;

        bool hasApply() const override;
        void
        apply(ExprHandle const & args, ContHandle cont, Env & env) override;

        bool isProcedure() const override;

    private:
        ExprHandle args;
        ExprHandle forms;
        Env env;
    };
}

#endif
