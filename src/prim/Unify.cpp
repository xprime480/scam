#include "prim/Unify.hpp"

#include "input/MatchUnifyParser.hpp"
#include "prim/MatchUnifyCommon.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;

void scam::applyUnify(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    static const char * myName = "unify";
    MatchUnifyParser * parser =
        standardMemoryManager.make<MatchUnifyParser>(false);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form form [dict])", args, cont, engine);
    }
    else {
        MatchUnifyCommon solver(parser, cont);
        solver.solve();
    }
}
