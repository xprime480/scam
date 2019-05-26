#include "expr/CarContinuation.hpp"

#include "WorkQueue.hpp"
#include "expr/MapCdr.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

CarContinuation::CarContinuation(WorkerData const & data)
    : Continuation("Cons Map Car")
    , data(data)
{
}

CarContinuation * CarContinuation::makeInstance(WorkerData const & data)
{
  return new CarContinuation(data);
}

void CarContinuation::mark() const
{
  if ( ! isMarked() ) {
      Continuation::mark();
      data.mark();
  }
}

void CarContinuation::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( error(expr) ) {
        data.original->run(expr);
    }
    else {
        ScamValue e = data.cdr;
        workQueueHelper<MapCdr>(expr, e, data.original, data.env);
    }
}
