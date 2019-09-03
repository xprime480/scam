#include "expr/CarContinuation.hpp"

#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/MapCdr.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

CarContinuation::CarContinuation(ScamValue cdr, Continuation * cont, Env * env)
    : Continuation("Cons Map Car")
    , cdr(cdr)
    , cont(cont)
    , env(env)
{
}

CarContinuation *
CarContinuation::makeInstance(ScamValue cdr, Continuation * cont, Env * env)
{
    return new CarContinuation(cdr, cont, env);
}

void CarContinuation::mark()
{
  if ( ! isMarked() ) {
      Continuation::mark();
      cdr->mark();
      cont->mark();
      env->mark();
  }
}

void CarContinuation::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        workQueueHelper<MapCdr>(value, cdr, cont, env);
    }
}
