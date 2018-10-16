
#include "form/EnvHelper.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

namespace
{
    class EnvHelperWorker : public Worker
    {
    public:
        EnvHelperWorker(ScamExpr * args,
                        ContHandle cont,
                        Env env,
                        char const * name);
        void run() override;

    protected:
        ContHandle cont;
        Env env;

        virtual ContHandle getCont(ScamExpr * sym) const = 0;

    private:
        ExprHandle args;
    };

    class AssignWorker : public EnvHelperWorker
    {
    public:
        AssignWorker(ScamExpr * args, ContHandle cont, Env env);

    protected:
        ContHandle getCont(ScamExpr * sym) const override;
    };

    class DefineWorker : public EnvHelperWorker
    {
    public:
        DefineWorker(ScamExpr * args, ContHandle cont, Env env);

    protected:
        ContHandle getCont(ScamExpr * sym) const override;
    };
}

EnvHelper::EnvHelper(char const * name)
    : SpecialForm(name)
{
}

Assign::Assign()
    : EnvHelper("assign")
{
}

void Assign::apply(ScamExpr * args, ContHandle cont, Env env)
{
    workQueueHelper<AssignWorker>(args, cont, env);
}

Define::Define()
    : EnvHelper("define")
{
}

void Define::apply(ScamExpr * args, ContHandle cont, Env env)
{
    workQueueHelper<DefineWorker>(args, cont, env);
}

namespace
{
    class EnvHelperCont : public Continuation
    {
    public:
        EnvHelperCont(ScamExpr * sym,
                      ContHandle cont,
                      Env env,
                      char const * name)
            : Continuation(name)
            , sym(sym->clone())
            , env(env)
            , cont(cont)
        {
        }

        void run(ScamExpr * expr) override
        {
            finish(expr);
            cont->run(expr);
        }

    protected:
        ExprHandle sym;
        mutable Env env;

        virtual void finish(ScamExpr * expr) const = 0;

    private:
        ContHandle cont;
    };

    class AssignCont : public EnvHelperCont
    {
    public:
        AssignCont(ScamExpr * sym, ContHandle cont, Env env)
            : EnvHelperCont(sym, cont, env, "Assign")
        {
        }

    protected:
        void finish(ScamExpr * expr) const override
        {
            env.assign(sym.get(), expr);
        }
    };

    class DefineCont : public EnvHelperCont
    {
    public:
        DefineCont(ScamExpr * sym, ContHandle cont, Env env)
            : EnvHelperCont(sym, cont, env, "Define")
        {
        }

    protected:
        void finish(ScamExpr * expr) const override
        {
            env.put(sym.get(), expr);
        }
    };

    EnvHelperWorker::EnvHelperWorker(ScamExpr * args,
                                     ContHandle cont,
                                     Env env,
                                     char const * name)
        : Worker(name)
        , cont(cont)
        , env(env)
        , args(args->clone())
    {
    }

    void EnvHelperWorker::run()
    {
        Worker::run();

        ExprHandle expr = args->getCdr()->getCar();
        ExprHandle sym = args->getCar();

        ContHandle c = getCont(sym.get());
        expr->eval(c, env);
    }

    AssignWorker::AssignWorker(ScamExpr * args,
                               ContHandle cont,
                               Env env)
        : EnvHelperWorker(args, cont, env, "Assign")
    {
    }

    ContHandle AssignWorker::getCont(ScamExpr * sym) const
    {
        return make_shared<AssignCont>(sym, cont, env);
    }

    DefineWorker::DefineWorker(ScamExpr * args,
                               ContHandle cont,
                               Env env)
        : EnvHelperWorker(args, cont, env, "Define")
    {
    }

    ContHandle DefineWorker::getCont(ScamExpr * sym) const
    {
        return make_shared<DefineCont>(sym, cont, env);
    }
}
