
#include "prim/Match.hpp"

#include "prim/MatchUnifyCommon.hpp"

using namespace scam;
using namespace std;

Match::Match()
    : Primitive("match")
{
}

Match * Match::makeInstance()
{
    return new Match();
}

void Match::applyArgs(ScamExpr * args, Continuation * cont)
{
    MatchUnifyCommon solver(args, cont, false);
    solver.solve();
}

