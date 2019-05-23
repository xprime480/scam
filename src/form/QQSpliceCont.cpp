#include "form/QQSpliceCont.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
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
    if ( TypePredicates::error(expr) ) {
        cont->run(expr);
    }
    else {
        ScamValue internal =
            ExpressionFactory::makeCons(QuasiQuote::spliceTag, expr);
        cont->run(internal);
    }
}
