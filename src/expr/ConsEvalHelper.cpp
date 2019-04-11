
#include "impl/ConsHelper.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ScamExpr.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace scam::cons_impl;
using namespace std;

namespace scam
{
    class MemoryManager;

    namespace cons_impl
    {
        class  ConsWorker : public Worker
        {
        private:
            friend class scam::MemoryManager;
            ConsWorker(Continuation * cont,
                       Env * env,
                       ScamExpr * car,
                       ScamExpr * cdr);

            ConsWorker(WorkerData const & data);

            static ConsWorker * makeInstance(Continuation * cont,
                                             Env * env,
                                             ScamExpr * car,
                                             ScamExpr * cdr);
            static ConsWorker * makeInstance(WorkerData const & data);

        public:
            void mark() const override;
            void run() override;

        private:
            WorkerData data;
        };

        void scamConsEvalHelper(ScamExpr * car,
                                ScamExpr * cdr,
                                Continuation * cont,
                                Env * env)
        {
            workQueueHelper<ConsWorker>(cont, env, car, cdr);
        }
    }
}

namespace
{
    class EvalContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        EvalContinuation(WorkerData const & data);
        static EvalContinuation * makeInstance(WorkerData const & data);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    private:
        WorkerData data;
    };
}

ConsWorker::ConsWorker(Continuation * cont,
                       Env * env,
                       ScamExpr * car,
                       ScamExpr * cdr)

    : Worker("Cons Eval")
    , data(car, cdr, cont, env)
{
    data.cont = standardMemoryManager.make<EvalContinuation>(data);
}

ConsWorker::ConsWorker(WorkerData const & data)
    : Worker("Cons Eval Copy")
    , data(data)
{
}

ConsWorker * ConsWorker::makeInstance(Continuation * cont,
                                      Env * env,
                                      ScamExpr * car,
                                      ScamExpr * cdr)

{
    return new ConsWorker(cont, env, car, cdr);
}

ConsWorker * ConsWorker::makeInstance(WorkerData const & data)
{
    return new ConsWorker(data);
}

void ConsWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        data.mark();
    }
}

void ConsWorker::run()
{
    Worker::run();
    data.car->eval(data.cont, data.env);
}

EvalContinuation::EvalContinuation(WorkerData const & data)
    : Continuation("Cons Eval Eval")
    , data(data)
{
}

EvalContinuation * EvalContinuation::makeInstance(WorkerData const & data)
{
    return new EvalContinuation(data);
}

void EvalContinuation::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        data.mark();
    }
}

void EvalContinuation::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        data.original->run(expr);
    }
    else {
        expr->apply(data.cdr, data.original, data.env);
    }
}
