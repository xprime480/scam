
#include "expr/ScamClass.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

namespace
{
    static const ExprHandle initSym = ExpressionFactory::makeSymbol("init");

    extern void do_apply(ScamExpr * base,
                         ScamExpr * vars,
                         ScamExpr * funs,
                         ScamExpr * args,
                         ContHandle cont,
                         Env env);
}

ScamClass::ScamClass(ScamExpr * base, ScamExpr * vars, ScamExpr * funs)
    : base(base->clone())
    , vars(vars->clone())
    , funs(funs->clone())
{
}

string ScamClass::toString() const
{
    return "class";
}

bool ScamClass::hasApply() const
{
    return true;
}

void ScamClass::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(base.get(), vars.get(), funs.get(), args, cont, env);
}

bool ScamClass::isProcedure() const
{
    return true;
}

bool ScamClass::isClass() const
{
    return true;
}

namespace
{
    class ClassInitCont: public Continuation
    {
    public:
        ClassInitCont(ScamExpr * instance, ContHandle cont)
            : Continuation("ClassInit")
            , instance(instance->clone())
            , cont(cont)
        {
        }

        void run(ScamExpr * expr) override
        {
            cont->run(instance.get());
        }

    private:
        ExprHandle instance;
        ContHandle cont;
    };

    class ClassInitWorker : public Worker
    {
    public:
        ClassInitWorker(ScamExpr * instance,
                        ScamExpr * args,
                        ContHandle cont,
                        Env env)
            : Worker("ClassInit")
            , instance(instance->clone())
            , args(args->clone())
            , cont(cont)
            , env(env)
        {
        }

        void run() override
        {
            Worker::run();
            ContHandle newCont
                = make_shared<ClassInitCont>(instance.get(), cont);
            ExprHandle newArgs
                = ExpressionFactory::makeCons(initSym.get(), args.get());

            instance->apply(newArgs.get(), newCont, env);
        }

    private:
        ExprHandle instance;
        ExprHandle args;
        ContHandle cont;
        Env        env;
    };

    class ClassCont : public Continuation
    {
    public:
        ClassCont(ScamExpr * base,
                  ScamExpr * vars,
                  ScamExpr * funs,
                  ContHandle cont,
                  Env env)
            : Continuation("ClassWorker")
            , base(base->clone())
            , vars(vars->clone())
            , funs(funs->clone())
            , cont(cont)
            , env(env)
        {
        }

        void run(ScamExpr * expr) override
        {
            if ( expr->error() ) {
                cont->run(expr);
            }
            else {
                ExprHandle instance
                    = ExpressionFactory::makeInstance(base.get(),
                                                      vars.get(),
                                                      funs.get(),
                                                      env);
                ScamExpr * i = instance.get();
                i->setSelf(i);
                workQueueHelper<ClassInitWorker>(i, expr, cont, env);
            }
        }

    private:
        ExprHandle base;
        ExprHandle vars;
        ExprHandle funs;
        ContHandle cont;
        Env        env;
    };

    class ClassWorker : public Worker
    {
    public:
        ClassWorker(ScamExpr * base,
                    ScamExpr * vars,
                    ScamExpr * funs,
                    ScamExpr * args,
                    ContHandle cont,
                    Env env)
            : Worker("ClassWorker")
            , base(base->clone())
            , vars(vars->clone())
            , funs(funs->clone())
            , args(args->clone())
            , cont(cont)
            , env(env)
        {
        }

        void run() override
        {
            Worker::run();
            ContHandle newCont
                = make_shared<ClassCont>(base.get(),
                                         vars.get(),
                                         funs.get(),
                                         cont,
                                         env);
            args->mapEval(newCont, env);
        }

    private:
        ExprHandle base;
        ExprHandle vars;
        ExprHandle funs;
        ExprHandle args;
        ContHandle cont;
        Env        env;
    };

    void do_apply(ScamExpr * base,
                  ScamExpr * vars,
                  ScamExpr * funs,
                  ScamExpr * args,
                  ContHandle cont,
                  Env env)
    {
        workQueueHelper<ClassWorker>(base, vars, funs, args, cont, env);
    }
}
