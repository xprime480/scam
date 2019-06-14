#include "form/QQSpliceCont.hpp"

#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AllSpecialForms.hpp"

using namespace scam;
using namespace std;

QQSpliceCont::QQSpliceCont(Continuation * cont, ScamEngine * engine)
    : Continuation("QQSpliceCont", engine)
    , cont(cont)
{
}

QQSpliceCont *
QQSpliceCont::makeInstance(Continuation * cont, ScamEngine * engine)
{
    return new QQSpliceCont(cont, engine);
}

void QQSpliceCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
    }
}

void QQSpliceCont::handleValue(ScamValue expr)
{
    Continuation::handleValue(expr);

    if ( isError(expr) ) {
        cont->handleValue(expr);
    }
    else {
        ScamValue internal = makePair(spliceTag, expr);
        cont->handleValue(internal);
    }
}
