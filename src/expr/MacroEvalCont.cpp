#include "expr/MacroEvalCont.hpp"

#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"

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

void MacroEvalCont::run(ScamValue expr)
{
    Continuation::run(expr);
    eval(expr, cont, capture);
}
