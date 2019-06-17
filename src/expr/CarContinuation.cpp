#include "expr/CarContinuation.hpp"

#include "ScamEngine.hpp"
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

void CarContinuation::mark()
{
  if ( ! isMarked() ) {
      Continuation::mark();
      data.mark();
  }
}

void CarContinuation::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isError(value) ) {
        engine->handleError(value);
    }
    else {
        ScamValue e = data.cdr;
        workQueueHelper<MapCdr>(value, e, data.original, data.env, engine);
    }
}
