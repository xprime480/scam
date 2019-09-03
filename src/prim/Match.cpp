#include "prim/Match.hpp"

#include "expr/ValueFactory.hpp"
#include "prim/MatchUnifyCommon.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applyMatch(ScamValue args, Continuation * cont)
{
    static const char * name = "match";
    ObjectParameter p0, p1;
    if ( argsToParms(args, name, p0, p1) ) {
        MatchUnifyCommon solver(p0.value, p1.value, makeDict(), true, cont);
        solver.solve();
    }
}
