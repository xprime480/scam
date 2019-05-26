#include "form/QQSpliceCont.hpp"

#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/QuasiQuote.hpp"

using namespace scam;
using namespace std;

QQSpliceCont::QQSpliceCont(Continuation * cont)
    : Continuation("QQSpliceCont")
    , cont(cont)
{
}

QQSpliceCont * QQSpliceCont::makeInstance(Continuation * cont)
{
    return new QQSpliceCont(cont);
}

void QQSpliceCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
    }
}

void QQSpliceCont::run(ScamValue expr)
{
    Continuation::run(expr);
    if ( error(expr) ) {
        cont->run(expr);
    }
    else {
        ScamValue internal = makeCons(QuasiQuote::spliceTag, expr);
        cont->run(internal);
    }
}
