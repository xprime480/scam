
#include "impl/ConsHelper.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>

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
                  Env env,
                  ScamExpr * car,
                  ScamExpr * cdr);

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
        void scamConsMapHelper(ScamExpr * car,
                               ScamExpr * cdr,
                               ContHandle cont,
                               Env env)
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
        MapCdr(ScamExpr * car,
               ScamExpr * cdr,
               ContHandle cont,
               Env env);

        void run() override;

    private:
        WorkerData data;
    };

    class CarContinuation : public Continuation
    {
    public:
        CarContinuation(WorkerData const & data);

        void run(ScamExpr * expr) override;

    private:
        WorkerData data;
    };

    class CdrContinuation : public Continuation
    {
    public:
        CdrContinuation(WorkerData const & data);

        void run(ScamExpr * expr) override;

    private:
        WorkerData data;
    };
}

MapWorker::MapWorker(ContHandle cont,
                     Env env,
                     ScamExpr * car,
                     ScamExpr * cdr)
    : Worker("Cons Map")
    , data(car, cdr, cont, env)
{
    data.cont = make_shared<CarContinuation>(data);
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

void CarContinuation::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        data.original->run(expr);
    }
    else {
        ScamExpr * e = data.cdr.get();
        workQueueHelper<MapCdr>(expr, e, data.original, data.env);
    }
}

MapCdr::MapCdr(ScamExpr * car, ScamExpr * cdr, ContHandle cont, Env env)
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

void CdrContinuation::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        data.original->run(expr);
    }
    else {
        ExprHandle x = ExpressionFactory::makeCons(data.car.get(), expr);
        ScamExpr * e = x.get();
        data.original->run(e);
    }
}

