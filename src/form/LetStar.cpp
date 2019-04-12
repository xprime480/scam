#include "form/LetStar.hpp"

#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/LetStarWorker.hpp"

using namespace scam;
using namespace std;

LetStar::LetStar(ScamEngine * engine)
    : SpecialForm("let*", true)
    , engine(engine)
{
}

LetStar * LetStar::makeInstance(ScamEngine * engine)
{
    return new LetStar(engine);
}

ScamExpr * LetStar::safeCons(ScamExpr * expr)
{
    if ( expr->isCons() ) {
        return expr;
    }
    return ExpressionFactory::makeList(expr);
}

void LetStar::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    workQueueHelper<LetStarWorker>(args, cont, env, engine);
}
