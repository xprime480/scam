#include "form/QQConsListCarCont.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "form/QQConsListCdrCont.hpp"
#include "form/QuasiQuoteWorker.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"

using namespace scam;
using namespace std;

QQConsListCarCont::QQConsListCarCont(ScamValue cdr,
                                     Continuation * cont,
                                     Env * env)
    : Continuation("QQConsListCarCont")
    , cdr(cdr)
    , cont(cont)
    , env(env)
{
}

QQConsListCarCont * QQConsListCarCont::makeInstance(ScamValue cdr,
                                                    Continuation * cont,
                                                    Env * env)
{
    return new QQConsListCarCont(cdr, cont, env);
}

void QQConsListCarCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cdr->mark();
        cont->mark();
        env->mark();
    }
}

void QQConsListCarCont::handleValue(ScamValue expr)
{
    Continuation::handleValue(expr);

    if ( isUnhandledError(expr) ) {
        ScamEngine::getEngine().handleError(expr);
    }
    else {
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Continuation * h = mm.make<QQConsListCdrCont>(expr, cont, env);
        workQueueHelper<QuasiQuoteWorker>(cdr, h, env);
    }
}
