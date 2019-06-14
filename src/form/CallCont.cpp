#include "form/CallCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

using namespace scam;
using namespace std;

CallCont::CallCont(Continuation * cont, Env * env, ScamEngine * engine)
    : Continuation("CallCont", engine)
    , cont(cont)
    , env(env)
{
}

CallCont *
CallCont::makeInstance(Continuation * cont, Env * env, ScamEngine * engine)
{
    return new CallCont(cont, env, engine);
}

void CallCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
        env->mark();
    }
}

void CallCont::run(ScamValue expr)
{
    if ( isError(expr) ) {
        cont->run(expr);
        return;
    }

    if ( ! isApplicable(expr) ) {
        ScamValue err = makeErrorExtended("call/cc: form ",
                                          writeValue(expr),
                                          "cannot be applied");
        cont->run(err);
        return;
    }

    ScamValue contExpr = makeContinuation(cont);
    ScamValue args = makeList(contExpr);
    apply(expr, args, cont, env, engine);
}
