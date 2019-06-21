#include "prim/WithHandlerCont.hpp"

#include "ScamEngine.hpp"

using namespace scam;
using namespace std;

WithHandlerCont::WithHandlerCont(Continuation * cont, ScamEngine * engine)
    : Continuation("WithHandler", engine)
    , cont(cont)
{
}

WithHandlerCont *
WithHandlerCont::makeInstance(Continuation * cont, ScamEngine * engine)
{
    return new WithHandlerCont(cont, engine);
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

    engine->popHandler();
    cont->handleValue(value);
}
