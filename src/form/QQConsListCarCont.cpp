#include "form/QQConsListCarCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "form/QQConsListCdrCont.hpp"
#include "form/QuasiQuote.hpp"
#include "form/QuasiQuoteWorker.hpp"
#include "util/MemoryManager.hpp"

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

QQConsListCarCont *
QQConsListCarCont::makeInstance(ScamValue cdr, Continuation * cont, Env * env)
{
    return new QQConsListCarCont(cdr, cont, env);
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

void QQConsListCarCont::run(ScamValue expr)
{
    Continuation::run(expr);
    if ( TypePredicates::error(expr) ) {
        cont->run(expr);
    }
    else {
        Continuation * h =
            standardMemoryManager.make<QQConsListCdrCont>(expr, cont, env);
        workQueueHelper<QuasiQuoteWorker>(cdr, h, env);
    }
}
