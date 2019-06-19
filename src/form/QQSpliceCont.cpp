#include "form/QQSpliceCont.hpp"

#include "ScamEngine.hpp"
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
        engine->handleError(value);
    }
    else {
        ScamValue internal = makePair(spliceTag, value);
        cont->handleValue(internal);
    }
}
