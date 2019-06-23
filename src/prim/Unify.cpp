#include "prim/Unify.hpp"

#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "prim/MatchUnifyCommon.hpp"
#include "util/Parameter.hpp"

using namespace scam;

void scam::applyUnify(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    static const char * name = "unify";
    ObjectParameter p0, p1;
    DictParameter pDict;
    OptionalParameter p2(pDict);
    if ( argsToParms(args, engine, name, p0, p1, p2) ) {
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
