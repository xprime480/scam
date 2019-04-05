
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

Apply * Apply::makeInstance()
{
    return new Apply();
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
            , op(op)
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
        ScamExpr * op;
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
            , op(op)
            , args(args)
            , cont(cont)
            , env(env)
        {
        }

        void run() override
        {
            ContHandle newCont = make_shared<ApplyArgsCont>(op, cont, env);
            args->eval(newCont, env);
        }

    private:
        ScamExpr * op;
        ScamExpr * args;
        ContHandle cont;
        Env        env;
    };

    class ApplyOpCont : public Continuation
    {
    public:
        ApplyOpCont(ScamExpr * args, ContHandle cont, Env env)
            : Continuation("apply")
            , args(args)
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
                workQueueHelper<ApplyArgsWorker>(expr, args, cont, env);
            }
        }

    private:
        ScamExpr * args;
        ContHandle cont;
        Env env;
    };

    void do_apply(ScamExpr * args, ContHandle cont, Env env)
    {
        ScamExpr * sym     = args->nthcar(0);
        ScamExpr * arglist = args->nthcar(1);
        ContHandle newCont = make_shared<ApplyOpCont>(arglist, cont, env);

        sym->eval(newCont, env);
    }
}
