
#include "impl/ConsHelper.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

using namespace scam;
using namespace scam::cons_impl;
using namespace std;

namespace scam
{
    namespace cons_impl
    {
        class  ConsWorker : public Worker
        {
        public:
            ConsWorker(ContHandle cont,
                       Env & env,
                       ExprHandle & car,
                       ExprHandle & cdr);

            ConsWorker(std::shared_ptr<WorkerData> data);

            void run() override;

        private:
            std::shared_ptr<WorkerData> data;
        };
    }

    namespace cons_impl
    {
        void scamConsEvalHelper(ExprHandle car,
                                ExprHandle cdr,
                                ContHandle cont,
                                Env & env)
        {
            workQueueHelper<ConsWorker>(cont, env, car, cdr);
        }
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

ConsWorker::ConsWorker(ContHandle cont,
                       Env & env,
                       ExprHandle & car,
                       ExprHandle & cdr)

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
