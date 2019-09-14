#include "form/QQSpliceCont.hpp"

#include "ScamEngine.hpp"
#include "form/AllSpecialForms.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

QQSpliceCont::QQSpliceCont(Continuation * cont)
    : Continuation("QQSpliceCont")
    , cont(cont)
{
}

QQSpliceCont *
QQSpliceCont::makeInstance(Continuation * cont)
{
    return new QQSpliceCont(cont);
}

void QQSpliceCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
    }
}

void QQSpliceCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        ScamValue internal = makePair(makeSymbol(spliceValue), value);
        cont->handleValue(internal);
    }
}
