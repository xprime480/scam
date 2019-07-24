#include "prim/PrimEvalCont.hpp"

#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

PrimEvalCont::PrimEvalCont(ScamValue caller,
                           Continuation * cont,
                           ScamEngine * engine)
    : Continuation("Primitive Eval", engine)
    , caller(caller)
    , cont(cont)
{
}

PrimEvalCont *
PrimEvalCont::makeInstance(ScamValue caller,
                           Continuation * cont,
                           ScamEngine * engine)
{
    return new PrimEvalCont(caller, cont, engine);
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
        engine->handleError(value);
    }
    else {
        (caller->primFunc())(value, cont, engine);
    }
}
