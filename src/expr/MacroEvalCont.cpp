#include "expr/MacroEvalCont.hpp"

#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"

using namespace scam;
using namespace std;

MacroEvalCont::MacroEvalCont(Continuation * cont,
                             Env * capture,
                             ScamEngine * engine)
    : Continuation("macro eval", engine)
    , cont(cont)
    , capture(capture)
{
}

MacroEvalCont * MacroEvalCont::makeInstance(Continuation * cont,
                                            Env * capture,
                                            ScamEngine * engine)
{
    return new MacroEvalCont(cont, capture, engine);
}

void MacroEvalCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
        capture->mark();
    }
}

void MacroEvalCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);
    eval(value, cont, capture, engine);
}
