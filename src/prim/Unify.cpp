
#include "prim/Unify.hpp"

#include "prim/MatchUnifyCommon.hpp"

using namespace scam;

Unify::Unify()
    : Primitive("unify")
{
}

Unify * Unify::makeInstance()
{
    return new Unify();
}

void Unify::applyArgs(ScamExpr * args, Continuation * cont)
{
    MatchUnifyCommon solver(args, cont, true);
    solver.solve();
}
