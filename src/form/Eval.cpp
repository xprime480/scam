
#include "form/Eval.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern void do_apply(ScamExpr * args, ContHandle cont, Env env);
}

Eval::Eval()
    : SpecialForm("eval")
{
}

Eval * Eval::makeInstance()
{
    static Eval instance;
    return &instance;
}

void Eval::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(args, cont, env);
}

namespace
{
    class EvalCont : public Continuation
    {
    public:
        EvalCont(ContHandle cont, Env env)
            : Continuation("eval")
            , cont(cont)
            , env(env)
        {
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);

            if ( expr->error() ) {
                cont->run(expr);
            }
            else {
                expr->eval(cont, env.top());
            }
        }

    private:
        ContHandle cont;
        Env env;
    };

    void do_apply(ScamExpr * args, ContHandle cont, Env env)
    {
        ContHandle finisher = make_shared<EvalCont>(cont, env);
        args->nthcar(0)->eval(finisher, env);
    }
}
