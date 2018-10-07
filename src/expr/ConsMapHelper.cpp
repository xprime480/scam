
#include "impl/ConsMapHelper.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace scam::cons_impl;
using namespace std;

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
    }
}

namespace
{
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

MapWorker::MapWorker(ExprHandle & car,
                     ExprHandle & cdr,
                     ContHandle cont,
                     Env & env)
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
        shared_ptr<MapCdr> thunk
            = make_shared<MapCdr>(expr, data->cdr, data->original, data->env);
        WorkerHandle start = thunk;
        GlobalWorkQueue.put(start);
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

