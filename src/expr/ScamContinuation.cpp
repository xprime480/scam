
#include "expr/ScamContinuation.hpp"

using namespace scam;
using namespace std;

ScamContinuation::ScamContinuation(ContHandle cont)
    : cont(cont.get())
    , cOld(cont)
{
}

ScamContinuation::ScamContinuation(Continuation * cont)
    : cont(cont)
{
}

ScamContinuation * ScamContinuation::makeInstance(ContHandle cont)
{
    return new ScamContinuation(cont);
}

ScamContinuation * ScamContinuation::makeInstance(Continuation * cont)
{
    return new ScamContinuation(cont);
}

void ScamContinuation::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        cont->mark();
    }
}

string ScamContinuation::toString() const
{
    static const string value { "continuation" };
    return value;
}

bool ScamContinuation::hasApply() const
{
    return true;
}

void ScamContinuation::apply(ScamExpr * args, ContHandle cont,  Env env)
{
    ScamExpr * arg = args->nthcar(0);
    this->cont->run(arg);
}
