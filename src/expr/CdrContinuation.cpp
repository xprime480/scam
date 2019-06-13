#include "expr/CdrContinuation.hpp"

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

void CdrContinuation::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        data.mark();
    }
}

void CdrContinuation::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( error(expr) ) {
        data.original->run(expr);
    }
    else {
        ScamValue e = makePair(data.car, expr);
        data.original->run(e);
    }
}
