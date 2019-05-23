#include "form/QQConsListCdrCont.hpp"

#include "Env.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "form/QuasiQuote.hpp"

using namespace scam;
using namespace std;

QQConsListCdrCont::QQConsListCdrCont(ExprHandle car,
                                     Continuation * cont,
                                     Env * env)
    : Continuation("QQConsListCdrCont")
    , car(car)
    , cont(cont)
    , env(env)
{
}

QQConsListCdrCont *
QQConsListCdrCont::makeInstance(ExprHandle car, Continuation * cont, Env * env)
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

void QQConsListCdrCont::run(ExprHandle expr)
{
    Continuation::run(expr);
    if ( TypePredicates::error(expr) ) {
        cont->run(expr);
    }
    else {
        handle(expr);
    }
}

void QQConsListCdrCont::handle(ExprHandle expr)
{
    if ( ! check_splice(expr) ) {
        ExprHandle rv = ExpressionFactory::makeCons(car, expr);
        cont->run(rv);
    }
}

bool QQConsListCdrCont::check_splice(ExprHandle expr)
{
    if ( TypePredicates::isCons(car) ) {
        ExprHandle first = car->nthcar(0);
        if ( TypePredicates::isSymbol(first) ) {
            if ( ExprWriter::write(first) ==
                 ExprWriter::write(QuasiQuote::spliceTag) ) {
                do_splice(expr);
                return true;
            }
        }
    }

    return false;
}

void QQConsListCdrCont::do_splice(ExprHandle expr)
{
    ExprHandle f = expr;
    size_t count = car->length();

    while ( --count > 0 ) {
        ExprHandle form = car->nthcar(count);
        f = ExpressionFactory::makeCons(form, f);
    }

    cont->run(f);
}

