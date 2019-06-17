#include "expr/CdrContinuation.hpp"

#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

CdrContinuation::CdrContinuation(WorkerData const & data, ScamEngine * engine)
    : Continuation("Cons Map Cdr", engine)
    , data(data)
{
}

CdrContinuation *
CdrContinuation::makeInstance(WorkerData const & data, ScamEngine * engine)
{
    return new CdrContinuation(data, engine);
}

void CdrContinuation::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        data.mark();
    }
}

void CdrContinuation::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isError(value) ) {
        engine->handleError(value);
    }
    else {
        ScamValue e = makePair(data.car, value);
        data.original->handleValue(e);
    }
}
