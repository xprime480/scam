
#include "form/Eval.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern void do_apply(ScamExpr * args, Continuation * cont, Env * env);
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

void Eval::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    do_apply(args, cont, env);
}

namespace
{
    class EvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        EvalCont(Continuation * cont, Env * env)
            : Continuation("eval")
            , cont(cont)
            , env(env)
        {
        }

        static EvalCont * makeInstance(Continuation * cont, Env * env)
        {
            return new EvalCont(cont, env);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                cont->mark();
                env->mark();
            }
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);

            if ( expr->error() ) {
                cont->run(expr);
            }
            else {
                expr->eval(cont, env->getTop());
            }
        }

    private:
        Continuation * cont;
        Env * env;
    };

    void do_apply(ScamExpr * args, Continuation * cont, Env * env)
    {
        Continuation * finisher =
            standardMemoryManager.make<EvalCont>(cont, env);
        args->nthcar(0)->eval(finisher, env);
    }
}
