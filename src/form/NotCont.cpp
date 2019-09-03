#include "form/NotCont.hpp"

#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

NotCont::NotCont(Continuation * cont)
    : Continuation("Not")
    , cont(cont)
{
}

NotCont * NotCont::makeInstance(Continuation * cont)
{
    return new NotCont(cont);
}

void NotCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
    }
}

void NotCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        ScamValue rv = makeBoolean(! truth(value));
        cont->handleValue(rv);
    }
}
