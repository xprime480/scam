#include "EvalWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"

using namespace scam;
using namespace std;

namespace
{
    class LocalEvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        LocalEvalCont(Continuation * cont)
            : Continuation("Local Eval")
            , cont(cont)
        {
        }

        static LocalEvalCont * makeInstance(Continuation * cont)
        {
            return new LocalEvalCont(cont);
        }

    public:
        void mark() override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                cont->mark();
            }
        }

        void handleValue(ScamValue value) override
        {
            Continuation::handleValue(value);

            if ( isList(value) && ! isNull(value) ) {
                unsigned len = length(value);
                ScamValue last = nthcar(value, len - 1);
                cont->handleValue(last);
            }
            else {
                cont->handleValue(value);
            }
        }

    private:
        Continuation * cont;
    };
}

EvalWorker::EvalWorker(ScamValue forms, Env * extended, Continuation * cont)
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

void EvalWorker::mark()
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

    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    LocalEvalCont * newCont = mm.make<LocalEvalCont>(cont);
    mapEval(forms, newCont, extended);
}
