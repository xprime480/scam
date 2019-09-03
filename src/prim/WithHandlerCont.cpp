#include "prim/WithHandlerCont.hpp"

#include "ScamEngine.hpp"

using namespace scam;
using namespace std;

WithHandlerCont::WithHandlerCont(Continuation * cont)
    : Continuation("WithHandler")
    , cont(cont)
{
}

WithHandlerCont *
WithHandlerCont::makeInstance(Continuation * cont)
{
    return new WithHandlerCont(cont);
}

void WithHandlerCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
    }
}

void WithHandlerCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    ScamEngine::getEngine().popHandler();
    cont->handleValue(value);
}
