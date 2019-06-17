#include "EvalWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
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
        EvalCont(Continuation * cont, ScamEngine * engine)
            : Continuation("eval", engine)
            , cont(cont)
        {
        }

        static EvalCont * makeInstance(Continuation * cont, ScamEngine * engine)
        {
            return new EvalCont(cont, engine);
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

EvalWorker::EvalWorker(ScamValue forms,
                       Env * extended,
                       Continuation * cont,
                       ScamEngine * engine)
    : Worker("eval", engine)
    , forms(forms)
    , extended(extended)
    , cont(cont)
{
}

EvalWorker * EvalWorker::makeInstance(ScamValue forms,
                                      Env * env,
                                      Continuation * cont,
                                      ScamEngine * engine)
{
    return new EvalWorker(forms, env, cont, engine);
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

    EvalCont * newCont = standardMemoryManager.make<EvalCont>(cont, engine);
    mapEval(forms, newCont, extended, engine);
}
