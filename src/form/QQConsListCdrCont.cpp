#include "form/QQConsListCdrCont.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "form/AllSpecialForms.hpp"

using namespace scam;
using namespace std;

QQConsListCdrCont::QQConsListCdrCont(ScamValue car,
                                     Continuation * cont,
                                     Env * env, ScamEngine * engine)
    : Continuation("QQConsListCdrCont", engine)
    , car(car)
    , cont(cont)
    , env(env)
{
}

QQConsListCdrCont * QQConsListCdrCont::makeInstance(ScamValue car,
                                                    Continuation * cont,
                                                    Env * env,
                                                    ScamEngine * engine)
{
    return new  QQConsListCdrCont(car, cont, env, engine);
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
        engine->handleError(value);
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
            if ( writeValue(first) == writeValue(spliceTag) ) {
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

