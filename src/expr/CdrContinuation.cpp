#include "expr/CdrContinuation.hpp"

#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

CdrContinuation::CdrContinuation(ScamValue car, Continuation * original)
    : Continuation("Cons Map Cdr")
    , car(car)
    , original(original)
{
}

CdrContinuation *
CdrContinuation::makeInstance(ScamValue car, Continuation * original)
{
    return new CdrContinuation(car, original);
}

void CdrContinuation::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        car->mark();
        original->mark();
    }
}

void CdrContinuation::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        ScamValue e = makePair(car, value);
        original->handleValue(e);
    }
}
