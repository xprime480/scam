
#include "prim/Backtrack.hpp"

#include "ScamEngine.hpp"
#include "Backtracker.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Backtrack::Backtrack(ScamEngine * engine)
    : Primitive("backtrack")
    , engine(engine)
{
}

Backtrack * Backtrack::makeInstance(ScamEngine * engine)
{
    return new Backtrack(engine);
}

void Backtrack::applyArgs(ScamExpr * args, Continuation * cont)
{
    Backtracker * backtracker = engine->getBacktracker();
    if ( ! backtracker ) {
	static const string msg = "No current backtrack context";
	static ScamExpr * rv = ExpressionFactory::makeError(msg, false);
	cont->run(rv);
    }
    else {
	backtracker->run();
    }
}