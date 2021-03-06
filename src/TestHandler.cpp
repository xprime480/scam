#include "TestHandler.hpp"

#include "Continuation.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

TestHandler::TestHandler(Continuation * cont)
    : Handler("Test Handler")
    , cont(cont)
{
}

TestHandler * TestHandler::makeInstance(Continuation * cont)
{
    return new TestHandler(cont);
}

void TestHandler::mark()
{
    if ( ! isMarked() ) {
        Handler::mark();
        cont->mark();
    }
}

ScamValue TestHandler::handleError(ScamValue err)
{
    Handler::handleError(err);
    cont->handleValue(err);
    return makeNothing();
}
