
#include "Binder.hpp"

#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

Binder::Binder(Env capture)
    : capture(capture)
{
}

Env Binder::bind(ScamExpr * formals, ScamExpr * actuals) const
{
    Env extended = capture.extend();
    bindOne(extended, formals, actuals);
    return extended;
}

void Binder::bindOne(Env env, ScamExpr * syms, ScamExpr * vals) const
{
    if ( syms->isCons() ) {
        env.put(syms->getCar(), vals->getCar());
        bindOne(env, syms->getCdr(), vals->getCdr());
    }
    else if ( ! syms->isNil() ) {
        env.put(syms, vals);
    }
}
