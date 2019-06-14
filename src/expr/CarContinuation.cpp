#include "expr/CarContinuation.hpp"

#include "WorkQueue.hpp"
#include "expr/MapCdr.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

CarContinuation::CarContinuation(WorkerData const & data, ScamEngine * engine)
    : Continuation("Cons Map Car", engine)
    , data(data)
{
}

CarContinuation *
CarContinuation::makeInstance(WorkerData const & data, ScamEngine * engine)
{
  return new CarContinuation(data, engine);
}

void CarContinuation::mark() const
{
  if ( ! isMarked() ) {
      Continuation::mark();
      data.mark();
  }
}

void CarContinuation::handleValue(ScamValue expr)
{
    Continuation::handleValue(expr);

    if ( isError(expr) ) {
        data.original->handleValue(expr);
    }
    else {
        ScamValue e = data.cdr;
        workQueueHelper<MapCdr>(expr, e, data.original, data.env, engine);
    }
}
