#if ! defined(SCAMCLOSURE_H)
#define SCAMCLOSURE_H 1

#include "Env.hpp"

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamClosure : public ScamExpr
    {
    private:
        ScamClosure(ScamExpr *formals,
                    ScamExpr * forms,
                    Env env,
                    bool macrolike = false);

    public:
        static ScamClosure * makeInstance(ScamExpr *formals,
                                          ScamExpr * forms,
                                          Env env,
                                          bool macrolike = false);

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ScamExpr * args, ContHandle cont, Env env) override;

        bool isProcedure() const override;

        ScamExpr * withEnvUpdate(Env updated) const override;

    private:
        ScamExpr * formals;
        ScamExpr * forms;
        Env env;
        bool macrolike;
    };
}

#endif
