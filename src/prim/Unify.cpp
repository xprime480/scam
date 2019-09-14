#include "prim/Unify.hpp"

#include "prim/MatchUnifyCommon.hpp"
#include "util/Parameter.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;

void scam::applyUnify(ScamValue args, Continuation * cont)
{
    static const char * name = "unify";
    ObjectParameter p0, p1;
    DictParameter pDict;
    OptionalParameter p2(pDict);
    if ( argsToParms(args, name, p0, p1, p2) ) {
        ScamValue dict;
        if ( isNothing(p2.value) ) {
            dict = makeDict();
        }
        else {
            dict = p2.value;
        }
        MatchUnifyCommon solver(p0.value, p1.value, dict, false, cont);
        solver.solve();
    }
}
