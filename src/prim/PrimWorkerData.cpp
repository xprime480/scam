#include "prim/PrimWorkerData.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

PrimWorkerData::PrimWorkerData(ScamValue args,
                               Continuation * original,
                               Env * env,
                               ScamValue caller)
    : args(args)
    , original(original)
    , cont(nullptr)
    , env(env)
    , caller(caller)
{
}

void PrimWorkerData::mark() const
{
    args->mark();
    original->mark();
    if ( cont ) { cont->mark(); };
    env->mark();
    caller->mark();
}

void PrimWorkerData::mapEval(ScamEngine * engine) const
{
    scam::mapEval(args, cont, env, engine);
}

void PrimWorkerData::handleResult(ScamValue expr, ScamEngine * engine)
{
    if ( isError(expr) ) {
        engine->handleError(expr);
    }
    else {
        PRIMFUNC(caller)(expr, original, PRIMENGINE(caller));
    }
}
