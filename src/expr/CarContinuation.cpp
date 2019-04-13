#include "expr/CarContinuation.hpp"

#include "expr/MapCdr.hpp"
#include "expr/ScamExpr.hpp"
#include "WorkQueue.hpp"

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

void CarContinuation::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        data.original->run(expr);
    }
    else {
        ScamExpr * e = data.cdr;
        workQueueHelper<MapCdr>(expr, e, data.original, data.env);
    }
}

