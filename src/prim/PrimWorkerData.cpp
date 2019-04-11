
#include "prim/PrimWorkerData.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "prim/Primitive.hpp"

using namespace scam;
using namespace std;

PrimWorkerData::PrimWorkerData(ScamExpr * args,
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

void PrimWorkerData::mapEval()
{
    args->mapEval(cont, env);
}

void PrimWorkerData::handleResult(ScamExpr * expr)
{
    if ( expr->error() ) {
        original->run(expr);
    }
    else {
        caller->applyArgs(expr, original);
    }
}

