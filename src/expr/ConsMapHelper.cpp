
#include "impl/ConsHelper.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace scam::cons_impl;
using namespace std;

#include "Worker.hpp"

#include <memory>

namespace
{
    class  MapWorker : public Worker
    {
    public:
        MapWorker(ContHandle cont,
                  Env & env,
                  ExprHandle & car,
                  ExprHandle & cdr);

        MapWorker(std::shared_ptr<WorkerData> data);

        void run() override;

    private:
        std::shared_ptr<WorkerData> data;
    };
}

namespace scam
{
    namespace cons_impl
    {
        void scamConsMapHelper(ExprHandle & car,
                               ExprHandle & cdr,
                               ContHandle cont,
                               Env & env)
        {
            workQueueHelper<MapWorker>(cont, env, car, cdr);
        }
    }
}

namespace
{
    class  MapCdr : public Worker
    {
    public:
        MapCdr(ExprHandle & car,
               ExprHandle & cdr,
               ContHandle cont,
               Env & env);

        MapCdr(std::shared_ptr<WorkerData> data);

        void run() override;

    private:
        std::shared_ptr<WorkerData> data;
    };

    class CarContinuation : public Continuation
    {
    public:
        CarContinuation(shared_ptr<WorkerData> data);

        void run(ExprHandle expr) const override;

    private:
        shared_ptr<WorkerData> data;
    };

    class CdrContinuation : public Continuation
    {
    public:
        CdrContinuation(shared_ptr<WorkerData> data);

        void run(ExprHandle expr) const override;

    private:
        shared_ptr<WorkerData> data;
    };
}

MapWorker::MapWorker(ContHandle cont,
                     Env & env,
                     ExprHandle & car,
                     ExprHandle & cdr)
    : data(make_shared<WorkerData>(car, cdr, cont, env))
{
    data->cont = make_shared<CarContinuation>(data);
}

MapWorker::MapWorker(shared_ptr<WorkerData> data)
    : data(data)
{
}

void MapWorker::run()
{
    data->car->eval(data->cont, data->env);
}

CarContinuation::CarContinuation(shared_ptr<WorkerData> data)
    : data(data)
{
}

void CarContinuation::run(ExprHandle expr) const
{
    if ( expr->error() ) {
        data->original->run(expr);
    }
    else {
        workQueueHelper<MapCdr>(expr, data->cdr, data->original, data->env);
    }
}

MapCdr::MapCdr(ExprHandle & car, ExprHandle & cdr, ContHandle cont, Env & env)
    : data(make_shared<WorkerData>(car, cdr, cont, env))
{
    data->cont = make_shared<CdrContinuation>(data);
}

MapCdr::MapCdr(shared_ptr<WorkerData> data)
    : data(data)
{
}

void MapCdr::run()
{
    data->cdr->mapEval(data->cont, data->env);
}

CdrContinuation::CdrContinuation(shared_ptr<WorkerData> data)
    : data(data)
{
}

void CdrContinuation::run(ExprHandle expr) const
{
    if ( ! expr->error() ) {
        expr = ExpressionFactory::makeCons(data->car, expr);
    }
    data->original->run(expr);
}

