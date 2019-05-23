#include "EvalWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

namespace
{
    class EvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        EvalCont(Continuation * cont)
            : Continuation("eval")
            , cont(cont)
        {
        }

        static EvalCont * makeInstance(Continuation * cont)
        {
            return new EvalCont(cont);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                cont->mark();
            }
        }

        void run(ScamValue expr) override
        {
            if ( TypePredicates::isList(expr) &&
                 ! TypePredicates::isNil(expr) ) {
                unsigned len = expr->length();
                ScamValue last = expr->nthcar(len - 1);
                cont->run(last);
            }
            else {
                cont->run(expr);
            }
        }

    private:
        Continuation * cont;
    };
}

EvalWorker::EvalWorker(ScamValue forms,
                       Env * extended,
                       Continuation * cont)
    : Worker("eval")
    , forms(forms)
    , extended(extended)
    , cont(cont)
{
}

EvalWorker *
EvalWorker::makeInstance(ScamValue forms, Env * env, Continuation * cont)
{
    return new EvalWorker(forms, env, cont);
}

void EvalWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        forms->mark();
        extended->mark();
        cont->mark();
    }
}

void EvalWorker::run()
{
    Worker::run();

    EvalCont * newCont = standardMemoryManager.make<EvalCont>(cont);
    forms->mapEval(newCont, extended);
}
