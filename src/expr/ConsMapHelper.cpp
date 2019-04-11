
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

namespace scam
{
    class MemoryManager;
}

namespace
{
    class  MapWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        MapWorker(Continuation * cont,
                  Env * env,
                  ScamExpr * car,
                  ScamExpr * cdr);

        static MapWorker * makeInstance(Continuation * cont,
                                        Env * env,
                                        ScamExpr * car,
                                        ScamExpr * cdr);

    public:
        void mark() const override;
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
                               Continuation * cont,
                               Env * env)
        {
            workQueueHelper<MapWorker>(cont, env, car, cdr);
        }
    }
}

namespace
{
    class  MapCdr : public Worker
    {
    private:
        friend class scam::MemoryManager;
        MapCdr(ScamExpr * car,
               ScamExpr * cdr,
               Continuation * cont,
               Env * env);

        static MapCdr * makeInstance(ScamExpr * car,
                                     ScamExpr * cdr,
                                     Continuation * cont,
                                     Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        WorkerData data;
    };

    class CarContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        CarContinuation(WorkerData const & data);
        static CarContinuation * makeInstance(WorkerData const & data);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    private:
        WorkerData data;
    };

    class CdrContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        CdrContinuation(WorkerData const & data);
        static CdrContinuation * makeInstance(WorkerData const & data);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    private:
        WorkerData data;
    };
}

MapWorker::MapWorker(Continuation * cont,
                     Env * env,
                     ScamExpr * car,
                     ScamExpr * cdr)
    : Worker("Cons Map")
    , data(car, cdr, cont, env)
{
    data.cont = standardMemoryManager.make<CarContinuation>(data);
}

MapWorker * MapWorker::makeInstance(Continuation * cont,
                                    Env * env,
                                    ScamExpr * car,
                                    ScamExpr * cdr)
{
    return new MapWorker(cont, env, car, cdr);
}

void MapWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        data.mark();
    }
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

CarContinuation * CarContinuation::makeInstance(WorkerData const & data)
{
  return new CarContinuation(data);
}

void CarContinuation::mark() const
{
  if ( ! isMarked() ) {
      Continuation::mark();
      data.mark();
  }
}

void CarContinuation::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        data.original->run(expr);
    }
    else {
        ScamExpr * e = data.cdr;
        workQueueHelper<MapCdr>(expr, e, data.original, data.env);
    }
}

MapCdr::MapCdr(ScamExpr * car, ScamExpr * cdr, Continuation * cont, Env * env)
    : Worker("Cons Map Cdr")
    , data(car, cdr, cont, env)
{
    data.cont = standardMemoryManager.make<CdrContinuation>(data);
}

MapCdr * MapCdr::makeInstance(ScamExpr * car,
                              ScamExpr * cdr,
                              Continuation * cont,
                              Env * env)
{
    return new MapCdr(car, cdr, cont, env);
}

void MapCdr::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        data.mark();
    }
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

CdrContinuation * CdrContinuation::makeInstance(WorkerData const & data)
{
    return new CdrContinuation(data);
}

void CdrContinuation::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        data.mark();
    }
}


void CdrContinuation::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        data.original->run(expr);
    }
    else {
        ScamExpr * e = ExpressionFactory::makeCons(data.car, expr);
        data.original->run(e);
    }
}

