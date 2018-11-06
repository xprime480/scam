
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

Env Binder::prebind(ScamExpr * formals) const
{
    Env extended = capture.extend();
    bindOne(extended, formals, formals);
    return extended;
}

void Binder::rebind(Env env, ScamExpr * formals, ScamExpr * actuals) const
{
    rebindOne(env, formals, actuals);
}

void Binder::bindOne(Env env, ScamExpr * syms, ScamExpr * vals) const
{
    if ( syms->isCons() ) {
        env.put(syms->getCar().get(), vals->getCar().get());
        bindOne(env, syms->getCdr().get(), vals->getCdr().get());
    }
    else if ( ! syms->isNil() ) {
        env.put(syms, vals);
    }
}

void Binder::rebindOne(Env env, ScamExpr * syms, ScamExpr * vals) const
{
    if ( syms->isCons() ) {
        ScamExpr * sym = syms->getCar().get();
        ScamExpr * val = vals->getCar().get();
        env.assign(sym, val);
        rebindOne(env, syms->getCdr().get(), vals->getCdr().get());
    }
    else if ( ! syms->isNil() ) {
        env.assign(syms, vals);
    }
}
