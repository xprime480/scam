
#include "EvalWorker.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
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

        void run(ScamExpr * expr) override
        {
            if ( expr->isList() && ! expr->isNil() ) {
                unsigned len = expr->length();
                ScamExpr * last = expr->nthcar(len - 1);
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

EvalWorker::EvalWorker(ScamExpr * forms, Env * extended, Continuation * cont)
    : Worker("eval")
    , forms(forms)
    , extended(extended)
    , cont(cont)
{
}

void EvalWorker::run()
{
    Worker::run();

    EvalCont * newCont = standardMemoryManager.make<EvalCont>(cont);
    forms->mapEval(newCont, extended);
}
