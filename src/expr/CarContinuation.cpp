#include "expr/CarContinuation.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "expr/MapCdr.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

CarContinuation::CarContinuation(ScamValue cdr,
                                 Continuation * cont,
                                 Env * env,
                                 ScamEngine * engine)
    : Continuation("Cons Map Car", engine)
    , cdr(cdr)
    , cont(cont)
    , env(env)
{
}

CarContinuation * CarContinuation::makeInstance(ScamValue cdr,
                                                Continuation * cont,
                                                Env * env,
                                                ScamEngine * engine)
{
    return new CarContinuation(cdr, cont, env, engine);
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
        engine->handleError(value);
    }
    else {
        workQueueHelper<MapCdr>(value, cdr, cont, env, engine);
    }
}
