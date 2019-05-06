#include "prim/Match.hpp"

#include "input/MatchUnifyParser.hpp"
#include "prim/MatchUnifyCommon.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "match";

Match::Match()
    : Primitive(myName)
{
}

Match * Match::makeInstance()
{
    return new Match();
}

void Match::applyArgs(ExprHandle args, Continuation * cont)
{
    MatchUnifyParser * parser =
        standardMemoryManager.make<MatchUnifyParser>(true);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form form)", args, cont);
    }
    else {
        MatchUnifyCommon solver(parser, cont);
        solver.solve();
    }
}

