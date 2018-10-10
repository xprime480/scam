
#include "form/Define.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

namespace
{
    class DefineHelper : public Worker
    {
    public:
        DefineHelper(ExprHandle const & args, ContHandle & cont, Env & env);
        void run() override;

    private:
        ExprHandle const args;
        ContHandle cont;
        Env & env;
    };
}

Define::Define()
    : SpecialForm("define")
{
}

void Define::apply(ExprHandle const & args, ContHandle cont, Env & env)
{
    workQueueHelper<DefineHelper>(args, cont, env);
}

namespace
{
    class DefineCont : public Continuation
    {
    public:
        DefineCont(ExprHandle const & sym, ContHandle & cont, Env & env)
            : Continuation("Define")
            , sym(sym)
            , cont(cont)
            , env(env)
        {
        }

        void run(ExprHandle expr) const override
        {
            Continuation::run(expr);
            ExprHandle key = sym;
            env.put(key, expr);
            cont->run(expr);
        }

    private:
        ExprHandle const sym;
        ContHandle cont;
        Env & env;
    };

    DefineHelper::DefineHelper(ExprHandle const & args,
                               ContHandle & cont,
                               Env & env)
        : Worker("Define")
        , args(args)
        , cont(cont)
        , env(env)
    {
    }

    void DefineHelper::run()
    {
        Worker::run();

        ExprHandle expr = args->getCdr()->getCar();
        ExprHandle sym = args->getCar();

        ContHandle c = make_shared<DefineCont>(sym, cont, env);
        expr->eval(c, env);
    }
}
