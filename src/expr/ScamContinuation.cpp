
#include "expr/ScamContinuation.hpp"

using namespace scam;
using namespace std;

ScamContinuation::ScamContinuation(ContHandle cont)
    : cont(cont)
{
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
    ExprHandle arg = args->nthcar(0);
    this->cont->run(arg.get());
}
