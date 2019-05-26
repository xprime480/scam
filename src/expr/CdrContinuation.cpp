#include "expr/CdrContinuation.hpp"

#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

CdrContinuation::CdrContinuation(WorkerData const & data)
    : Continuation("Cons Map Cdr")
    , data(data)
{
}

CdrContinuation * CdrContinuation::makeInstance(WorkerData const & data)
{
    return new CdrContinuation(data);
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
        ScamValue e = makeCons(data.car, expr);
        data.original->run(e);
    }
}
