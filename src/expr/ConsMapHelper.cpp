
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

        MapWorker(WorkerData const & data);

        void run() override;

    private:
        WorkerData data;
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
        MapCdr(ExprHandle const & car,
               ExprHandle const & cdr,
               ContHandle cont,
               Env & env);

        void run() override;

    private:
        WorkerData data;
    };

    class CarContinuation : public Continuation
    {
    public:
        CarContinuation(WorkerData const & data);

        void run(ExprHandle expr) const override;

    private:
        WorkerData data;
    };

    class CdrContinuation : public Continuation
    {
    public:
        CdrContinuation(WorkerData const & data);

        void run(ExprHandle expr) const override;

    private:
        WorkerData data;
    };
}

MapWorker::MapWorker(ContHandle cont,
                     Env & env,
                     ExprHandle & car,
                     ExprHandle & cdr)
    : Worker("Cons Map")
    , data(car, cdr, cont, env)
{
    data.cont = make_shared<CarContinuation>(data);
}

MapWorker::MapWorker(WorkerData const & data)
    : Worker("Cons Map Copy")
    , data(data)
{
}

void MapWorker::run()
{
    Worker::run();
    data.car->eval(data.cont, data.env);
}

CarContinuation::CarContinuation(WorkerData const & data)
    : Continuation("Cons Map Car")
    , data(data)
{
}

void CarContinuation::run(ExprHandle expr) const
{
    Continuation::run(expr);

    if ( expr->error() ) {
        data.original->run(expr);
    }
    else {
        workQueueHelper<MapCdr>(expr, data.cdr, data.original, data.env);
    }
}

MapCdr::MapCdr(ExprHandle const & car, ExprHandle const & cdr, ContHandle cont, Env & env)
    : Worker("Cons Map Cdr")
    , data(car, cdr, cont, env)
{
    data.cont = make_shared<CdrContinuation>(data);
}

void MapCdr::run()
{
    Worker::run();
    data.cdr->mapEval(data.cont, data.env);
}

CdrContinuation::CdrContinuation(WorkerData const & data)
    : Continuation("Cons Map Cdr")
    , data(data)
{
}

void CdrContinuation::run(ExprHandle expr) const
{
    Continuation::run(expr);

    if ( ! expr->error() ) {
        expr = ExpressionFactory::makeCons(data.car, expr);
    }
    data.original->run(expr);
}

