#include "form/QQConsListCdrCont.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/QuasiQuote.hpp"

using namespace scam;
using namespace std;

QQConsListCdrCont::QQConsListCdrCont(ScamExpr * car,
                                     Continuation * cont,
                                     Env * env)
    : Continuation("QQConsListCdrCont")
    , car(car)
    , cont(cont)
    , env(env)
{
}

QQConsListCdrCont *
QQConsListCdrCont::makeInstance(ScamExpr * car, Continuation * cont, Env * env)
{
    return new  QQConsListCdrCont(car, cont, env);
}

void QQConsListCdrCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        car->mark();
        cont->mark();
        env->mark();
    }
}

void QQConsListCdrCont::run(ScamExpr * expr)
{
    Continuation::run(expr);
    if ( expr->error() ) {
        cont->run(expr);
    }
    else {
        handle(expr);
    }
}

void QQConsListCdrCont::handle(ScamExpr * expr)
{
    if ( ! check_splice(expr) ) {
        ScamExpr * rv = ExpressionFactory::makeCons(car, expr);
        cont->run(rv);
    }
}

bool QQConsListCdrCont::check_splice(ScamExpr * expr)
{
    if ( car->isCons() ) {
        ScamExpr * first = car->nthcar(0);
        if ( first->isSymbol() ) {
            if ( first->toString() == QuasiQuote::spliceTag->toString() ) {
                do_splice(expr);
                return true;
            }
        }
    }

    return false;
}

void QQConsListCdrCont::do_splice(ScamExpr * expr)
{
    ScamExpr * f = expr;
    size_t count = car->length();

    while ( --count > 0 ) {
        ScamExpr * form = car->nthcar(count);
        f = ExpressionFactory::makeCons(form, f);
    }

    cont->run(f);
}

