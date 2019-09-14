#include "form/QQConsListCdrCont.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/SequenceOps.hpp"
#include "form/AllSpecialForms.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

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

QQConsListCdrCont * QQConsListCdrCont::makeInstance(ScamValue car,
                                                    Continuation * cont,
                                                    Env * env)
{
    return new  QQConsListCdrCont(car, cont, env);
}

void QQConsListCdrCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        car->mark();
        cont->mark();
        env->mark();
    }
}

void QQConsListCdrCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        handle(value);
    }
}

void QQConsListCdrCont::handle(ScamValue expr)
{
    if ( ! check_splice(expr) ) {
        ScamValue rv = makePair(car, expr);
        cont->handleValue(rv);
    }
}

bool QQConsListCdrCont::check_splice(ScamValue expr)
{
    if ( isPair(car) ) {
        ScamValue first = nthcar(car, 0);
        if ( isSymbol(first) ) {
	    const ScamValue spliceTag = makeSymbol(spliceValue);
            if ( equals(first, spliceTag) ) {
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
        f = makePair(form, f);
    }

    cont->handleValue(f);
}

