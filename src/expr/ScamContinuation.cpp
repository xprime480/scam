
#include "expr/ScamContinuation.hpp"

using namespace scam;
using namespace std;

ScamContinuation::ScamContinuation(ContHandle cont)
    : cont(cont)
{
}

ScamContinuation * ScamContinuation::makeInstance(ContHandle cont)
{
    return new ScamContinuation(cont);
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
