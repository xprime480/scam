#include "prim/WithHandlerCont.hpp"

#include "ScamEngine.hpp"

#include "util/GlobalId.hpp"
#include "util/DebugTrace.hpp"
#include "expr/ValueWriter.hpp"

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
    GlobalId id;
    ScamTraceScope _;
    
    scamTrace(id, __FILE__, __LINE__, __FUNCTION__, writeValue(value));
    Continuation::handleValue(value);

    scamTrace(id, __FILE__, __LINE__, __FUNCTION__);
    engine->popHandler();
    scamTrace(id, __FILE__, __LINE__, __FUNCTION__);
    cont->handleValue(value);
    scamTrace(id, __FILE__, __LINE__, __FUNCTION__);
}
