
#include "form/Apply.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>

using namespace scam;
using namespace std;

namespace
{
    extern void do_apply(ScamExpr * args, ContHandle cont, Env env);
}

Apply::Apply()
    : SpecialForm("apply")
{
}

void Apply::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(args, cont, env);
}

namespace
{
    class ApplyArgsCont : public Continuation
    {
    public:
        ApplyArgsCont(ScamExpr * op, ContHandle cont, Env env)
            : Continuation("apply args")
            , op(op->clone())
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
                op->apply(expr, cont, env);
            }
        }

    private:
        ExprHandle op;
        ContHandle cont;
        Env env;
    };

    class ApplyArgsWorker : public Worker
    {
    public:
        ApplyArgsWorker(ScamExpr * op,
                        ScamExpr * args,
                        ContHandle cont,
                        Env env)
            : Worker("Apply Args")
            , op(op->clone())
            , args(args->clone())
            , cont(cont)
            , env(env)
        {
        }

        void run() override
        {
            ContHandle newCont
                = make_shared<ApplyArgsCont>(op.get(), cont, env);
            args->eval(newCont, env);
        }

    private:
        ExprHandle op;
        ExprHandle args;
        ContHandle cont;
        Env        env;
    };

    class ApplyOpCont : public Continuation
    {
    public:
        ApplyOpCont(ScamExpr * args, ContHandle cont, Env env)
            : Continuation("apply")
            , args(args->clone())
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
                ScamExpr * a = args.get();
                workQueueHelper<ApplyArgsWorker>(expr, a, cont, env);
            }
        }

    private:
        ExprHandle args;
        ContHandle cont;
        Env env;
    };

    void do_apply(ScamExpr * args, ContHandle cont, Env env)
    {
        ExprHandle sym     = args->nthcar(0);
        ExprHandle arglist = args->nthcar(1);
        ContHandle newCont = make_shared<ApplyOpCont>(arglist.get(), cont, env);

        sym->eval(newCont, env);
    }
}
