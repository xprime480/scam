
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
        EnvHelperWorker(ExprHandle const & args,
                        ContHandle & cont,
                        Env & env,
                        char const * name);
        void run() override;

    protected:
        ContHandle cont;
        Env & env;

        virtual ContHandle getCont(ExprHandle const & sym) const = 0;

    private:
        ExprHandle const args;
    };

    class AssignWorker : public EnvHelperWorker
    {
    public:
        AssignWorker(ExprHandle const & args, ContHandle & cont, Env & env);

    protected:
        ContHandle getCont(ExprHandle const & sym) const override;
    };

    class DefineWorker : public EnvHelperWorker
    {
    public:
        DefineWorker(ExprHandle const & args, ContHandle & cont, Env & env);

    protected:
        ContHandle getCont(ExprHandle const & sym) const override;
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

void Assign::apply(ExprHandle const & args, ContHandle cont, Env & env)
{
    workQueueHelper<AssignWorker>(args, cont, env);
}

Define::Define()
    : EnvHelper("define")
{
}

void Define::apply(ExprHandle const & args, ContHandle cont, Env & env)
{
    workQueueHelper<DefineWorker>(args, cont, env);
}

namespace
{
    class EnvHelperCont : public Continuation
    {
    public:
        EnvHelperCont(ExprHandle const & sym,
                      ContHandle cont,
                      Env & env,
                      char const * name)
            : Continuation(name)
            , sym(sym)
            , env(env)
            , cont(cont)
        {
        }

        void run(ExprHandle expr) const override
        {
            Continuation::run(expr);
            ExprHandle key = sym;
            finish(expr);
            cont->run(expr);
        }

    protected:
        ExprHandle const sym;
        Env & env;

        virtual void finish(ExprHandle const & expr) const = 0;

    private:
        ContHandle cont;
    };

    class AssignCont : public EnvHelperCont
    {
    public:
        AssignCont(ExprHandle const & sym, ContHandle cont, Env & env)
            : EnvHelperCont(sym, cont, env, "Assign")
        {
        }

    protected:
        void finish(ExprHandle const & expr) const override
        {
            env.assign(sym, expr);
        }
    };

    class DefineCont : public EnvHelperCont
    {
    public:
        DefineCont(ExprHandle const & sym, ContHandle cont, Env & env)
            : EnvHelperCont(sym, cont, env, "Define")
        {
        }

    protected:
        void finish(ExprHandle const & expr) const override
        {
            env.put(sym, expr);
        }
    };

    EnvHelperWorker::EnvHelperWorker(ExprHandle const & args,
                                     ContHandle & cont,
                                     Env & env,
                                     char const * name)
        : Worker(name)
        , cont(cont)
        , env(env)
        , args(args)
    {
    }

    void EnvHelperWorker::run()
    {
        Worker::run();

        ExprHandle expr = args->getCdr()->getCar();
        ExprHandle sym = args->getCar();

        ContHandle c = getCont(sym);
        expr->eval(c, env);
    }

    AssignWorker::AssignWorker(ExprHandle const & args,
                               ContHandle & cont,
                               Env & env)
        : EnvHelperWorker(args, cont, env, "Assign")
    {
    }

    ContHandle AssignWorker::getCont(ExprHandle const & sym) const
    {
        return make_shared<AssignCont>(sym, cont, env);
    }

    DefineWorker::DefineWorker(ExprHandle const & args,
                               ContHandle & cont,
                               Env & env)
        : EnvHelperWorker(args, cont, env, "Define")
    {
    }

    ContHandle DefineWorker::getCont(ExprHandle const & sym) const
    {
        return make_shared<DefineCont>(sym, cont, env);
    }
}
