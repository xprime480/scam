#include "form/QQConsListCarCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "form/QuasiQuote.hpp"
#include "form/QQConsListCdrCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

QQConsListCarCont::QQConsListCarCont(ScamExpr * cdr,
                                     Continuation * cont,
                                     Env * env)
    : Continuation("QQConsListCarCont")
    , cdr(cdr)
    , cont(cont)
    , env(env)
{
}

QQConsListCarCont *
QQConsListCarCont::makeInstance(ScamExpr * cdr, Continuation * cont, Env * env)
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

void QQConsListCarCont::run(ScamExpr * expr)
{
    Continuation::run(expr);
    if ( expr->error() ) {
        cont->run(expr);
    }
    else {
        Continuation * h =
            standardMemoryManager.make<QQConsListCdrCont>(expr,
                                                          cont,
                                                          env);
        QuasiQuote::qq_apply(cdr, h, env, false);
    }
}
