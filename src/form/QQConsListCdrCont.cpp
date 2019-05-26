#include "form/QQConsListCdrCont.hpp"

#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "form/QuasiQuote.hpp"

using namespace scam;
using namespace std;

QQConsListCdrCont::QQConsListCdrCont(ScamValue car,
                                     Continuation * cont,
                                     Env * env)
    : Continuation("QQConsListCdrCont")
    , car(car)
    , cont(cont)
    , env(env)
{
}

QQConsListCdrCont *
QQConsListCdrCont::makeInstance(ScamValue car, Continuation * cont, Env * env)
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

void QQConsListCdrCont::run(ScamValue expr)
{
    Continuation::run(expr);
    if ( error(expr) ) {
        cont->run(expr);
    }
    else {
        handle(expr);
    }
}

void QQConsListCdrCont::handle(ScamValue expr)
{
    if ( ! check_splice(expr) ) {
        ScamValue rv = ExpressionFactory::makeCons(car, expr);
        cont->run(rv);
    }
}

bool QQConsListCdrCont::check_splice(ScamValue expr)
{
    if ( isCons(car) ) {
        ScamValue first = nthcar(car, 0);
        if ( isSymbol(first) ) {
            if ( writeValue(first) == writeValue(QuasiQuote::spliceTag) ) {
                do_splice(expr);
                return true;
            }
        }
    }

    return false;
}

void QQConsListCdrCont::do_splice(ScamValue expr)
{
    ScamValue f = expr;
    size_t count = length(car);

    while ( --count > 0 ) {
        ScamValue form = nthcar(car, count);
        f = ExpressionFactory::makeCons(form, f);
    }

    cont->run(f);
}

