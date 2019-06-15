#include "form/QQConsListCarCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "form/QQConsListCdrCont.hpp"
#include "form/QuasiQuoteWorker.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

QQConsListCarCont::QQConsListCarCont(ScamValue cdr,
                                     Continuation * cont,
                                     Env * env,
                                     ScamEngine * engine)
    : Continuation("QQConsListCarCont", engine)
    , cdr(cdr)
    , cont(cont)
    , env(env)
{
}

QQConsListCarCont * QQConsListCarCont::makeInstance(ScamValue cdr,
                                                    Continuation * cont,
                                                    Env * env,
                                                    ScamEngine * engine)
{
    return new QQConsListCarCont(cdr, cont, env, engine);
}

void QQConsListCarCont::mark() const
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
    if ( isError(expr) ) {
        engine->handleError(expr);
    }
    else {
        Continuation * h =
            standardMemoryManager.make<QQConsListCdrCont>(expr,
                                                          cont,
                                                          env,
                                                          engine);
        workQueueHelper<QuasiQuoteWorker>(cdr, h, env, engine);
    }
}
