#include "TestHandler.hpp"

#include "Continuation.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

TestHandler::TestHandler(Continuation * cont)
    : cont(cont)
{
}

ScamValue TestHandler::handleError(ScamValue err)
{
    cont->handleValue(err);     // give it to backstop continuation
    return makeNothing();       // and terminate further processing.
}
