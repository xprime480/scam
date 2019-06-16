#include "prim/Match.hpp"

#include "input/MatchUnifyParser.hpp"
#include "prim/MatchUnifyCommon.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

void scam::applyMatch(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    static const char * myName = "match";

    MatchUnifyParser * parser =
        standardMemoryManager.make<MatchUnifyParser>(true);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form form)", args, cont, engine);
    }
    else {
        MatchUnifyCommon solver(parser, cont);
        solver.solve();
    }
}

