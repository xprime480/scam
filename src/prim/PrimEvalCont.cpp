#include "prim/PrimEvalCont.hpp"

#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

PrimEvalCont::PrimEvalCont(ScamValue caller, Continuation * cont)
    : Continuation("Primitive Eval")
    , caller(caller)
    , cont(cont)
{
}

PrimEvalCont * PrimEvalCont::makeInstance(ScamValue caller, Continuation * cont)
{
    return new PrimEvalCont(caller, cont);
}

void PrimEvalCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        caller->mark();
        cont->mark();
    }
}

void PrimEvalCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        (caller->primFunc())(value, cont);
    }
}
