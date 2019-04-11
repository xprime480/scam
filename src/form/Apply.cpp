
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
    extern void do_apply(ScamExpr * args, Continuation * cont, Env * env);
}

Apply::Apply()
    : SpecialForm("apply")
{
}

Apply * Apply::makeInstance()
{
    static Apply instance;
    return &instance;
}

void Apply::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    do_apply(args, cont, env);
}

namespace
{
    class ApplyArgsCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ApplyArgsCont(ScamExpr * op, Continuation * cont, Env * env)
            : Continuation("apply args")
            , op(op)
            , cont(cont)
            , env(env)
        {
        }

        static ApplyArgsCont *
        makeInstance(ScamExpr * op, Continuation * cont, Env * env)
        {
            return new ApplyArgsCont(op, cont, env);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                op->mark();
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
                op->apply(expr, cont, env);
            }
        }

    private:
        ScamExpr * op;
        Continuation * cont;
        Env * env;
    };

    class ApplyArgsWorker : public Worker
    {
    public:
        ApplyArgsWorker(ScamExpr * op,
                        ScamExpr * args,
                        Continuation * cont,
                        Env * env)
            : Worker("Apply Args")
            , op(op)
            , args(args)
            , cont(cont)
            , env(env)
        {
        }

        void run() override
        {
            Continuation * newCont =
                standardMemoryManager.make<ApplyArgsCont>(op, cont, env);
            args->eval(newCont, env);
        }

    private:
        ScamExpr * op;
        ScamExpr * args;
        Continuation * cont;
        Env *        env;
    };

    class ApplyOpCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ApplyOpCont(ScamExpr * args, Continuation * cont, Env * env)
            : Continuation("apply")
            , args(args)
            , cont(cont)
            , env(env)
        {
        }

        static ApplyOpCont *
        makeInstance(ScamExpr * args, Continuation * cont, Env * env)
        {
            return new ApplyOpCont(args, cont, env);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                args->mark();
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
                workQueueHelper<ApplyArgsWorker>(expr, args, cont, env);
            }
        }

    private:
        ScamExpr * args;
        Continuation * cont;
        Env * env;
    };

    void do_apply(ScamExpr * args, Continuation * cont, Env * env)
    {
        ScamExpr * sym     = args->nthcar(0);
        ScamExpr * arglist = args->nthcar(1);
        Continuation * newCont =
            standardMemoryManager.make<ApplyOpCont>(arglist, cont, env);

        sym->eval(newCont, env);
    }
}
