#include "expr/MacroEvalCont.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

MacroEvalCont::MacroEvalCont(Continuation * cont, Env * capture)
    : Continuation("macro eval")
    , cont(cont)
    , capture(capture)
{
}

MacroEvalCont *
MacroEvalCont::makeInstance(Continuation * cont, Env * capture)
{
    return new MacroEvalCont(cont, capture);
}

void MacroEvalCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
        capture->mark();
    }
}

void MacroEvalCont::run(ScamExpr * expr)
{
    Continuation::run(expr);
    expr->eval(cont, capture);
}