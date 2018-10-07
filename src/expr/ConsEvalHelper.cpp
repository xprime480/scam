
#include "impl/ConsEvalHelper.hpp"

#include "Continuation.hpp"
#include "Env.hpp"

using namespace scam;
using namespace scam::cons_impl;
using namespace std;

namespace
{
    class Evaluator : public Continuation
    {
    public:
        Evaluator(ExprHandle args, ContHandle cont, Env env)
            : args(args)
            , cont(cont)
            , env(env)
        {
        }

        void run(ExprHandle e) const override
        {
            e->apply(args, cont, env);
        }

    private:
        mutable ExprHandle args;
        mutable ContHandle cont;
        mutable Env env;
    };
}

namespace scam
{
    namespace cons_impl
    {
        struct WorkerData
        {
            WorkerData(ExprHandle car,
                           ExprHandle cdr,
                           ContHandle original,
                           Env & env)
                : car(car)
                , cdr(cdr)
                , original(original)
                , env(env)
            {
            }

            ExprHandle car;
            ExprHandle cdr;
            ContHandle original;
            ContHandle cont;
            Env & env;
        };
    }
}

namespace
{
    class EvalContinuation : public Continuation
    {
    public:
        EvalContinuation(shared_ptr<WorkerData> data);

        void run(ExprHandle expr) const override;

    private:
        shared_ptr<WorkerData> data;
    };
}

ConsWorker::ConsWorker(ExprHandle & car,
                       ExprHandle & cdr,
                       ContHandle cont,
                       Env & env)
    : data(make_shared<WorkerData>(car, cdr, cont, env))
{
    data->cont = make_shared<EvalContinuation>(data);
}

ConsWorker::ConsWorker(shared_ptr<WorkerData> data)
    : data(data)
{
}

void ConsWorker::run()
{
    data->car->eval(data->cont, data->env);
}

EvalContinuation::EvalContinuation(shared_ptr<WorkerData> data)
    : data(data)
{
}

void EvalContinuation::run(ExprHandle expr) const
{
    if ( expr->error() ) {
        data->original->run(expr);
    }
    else {
        expr->apply(data->cdr, data->original, data->env);
    }
}
