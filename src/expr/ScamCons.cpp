#include "expr/ScamCons.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ConsWorker.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/MapWorker.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

using namespace scam;
using namespace std;

ScamCons::ScamCons(ScamValue car, ScamValue cdr)
    : ScamExpr(ScamData::Cons)
{
    CAR(this) = car;
    CDR(this) = cdr;
}

ScamCons * ScamCons::makeInstance(ScamValue car, ScamValue cdr)
{
    return new ScamCons(car, cdr);
}

void ScamCons::eval(Continuation * cont, Env * env) const
{
    workQueueHelper<ConsWorker>(cont, env, CAR(this), CDR(this));
}

void ScamCons::mapEval(Continuation * cont, Env * env) const
{
    workQueueHelper<MapWorker>(cont, env, CAR(this), CDR(this));
}
