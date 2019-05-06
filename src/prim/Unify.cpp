
#include "prim/Unify.hpp"

#include "input/MatchUnifyParser.hpp"
#include "prim/MatchUnifyCommon.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;

static const char * myName = "unify";

Unify::Unify()
    : Primitive(myName)
{
}

Unify * Unify::makeInstance()
{
    return new Unify();
}

void Unify::applyArgs(ExprHandle args, Continuation * cont)
{
    MatchUnifyParser * parser =
        standardMemoryManager.make<MatchUnifyParser>(false);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form form [dict])", args, cont);
    }
    else {
        MatchUnifyCommon solver(parser, cont);
        solver.solve();
    }
}
