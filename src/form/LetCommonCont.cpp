#include "form/LetCommonCont.hpp"

#include "Continuation.hpp"
#include "EvalWorker.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

LetCommonCont::LetCommonCont(char const * name,
                             ScamValue forms,
                             Continuation * cont,
                             ScamEngine * engine)
    : Continuation(name, engine)
    , forms(forms)
    , cont(cont)
{
}

void LetCommonCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        forms->mark();
        cont->mark();
    }
}

void LetCommonCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isError(value) ) {
        engine->handleError(value);
    }
    else {
        do_let(value);
    }
}

void LetCommonCont::final_eval(Env * env)
{
    workQueueHelper<EvalWorker>(forms, env, cont, engine);
}

