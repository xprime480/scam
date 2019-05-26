#include "prim/PrimWorkerData.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "prim/Primitive.hpp"

using namespace scam;
using namespace std;

PrimWorkerData::PrimWorkerData(ScamValue args,
                               Continuation * original,
                               Env * env,
                               Primitive * caller)
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
}

void PrimWorkerData::mapEval() const
{
    scam::mapEval(args, cont, env);
}

void PrimWorkerData::handleResult(ScamValue expr)
{
    if ( error(expr) ) {
        original->run(expr);
    }
    else {
        caller->applyArgs(expr, original);
    }
}

