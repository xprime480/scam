
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
    namespace cons_impl
    {
        class  ConsWorker : public Worker
        {
        public:
            ConsWorker(Continuation * cont,
                       Env env,
                       ScamExpr * car,
                       ScamExpr * cdr);

            ConsWorker(WorkerData const & data);

            void run() override;

        private:
            WorkerData data;
        };
    }

    namespace cons_impl
    {
        void scamConsEvalHelper(ScamExpr * car,
                                ScamExpr * cdr,
                                Continuation * cont,
                                Env env)
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
                       Env env,
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
