#include "Binder.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ScamSymbol.hpp"

using namespace scam;
using namespace std;

Binder::Binder(Env * capture)
    : capture(capture)
{
}

Env * Binder::bind(ExprHandle formals, ExprHandle actuals) const
{
    Env * extended = capture->extend();
    bindOne(extended, formals, actuals);
    return extended;
}

void Binder::bindOne(Env * env, ExprHandle syms, ExprHandle vals) const
{
    if ( syms->isCons() ) {
        env->put(syms->getCar(), vals->getCar());
        bindOne(env, syms->getCdr(), vals->getCdr());
    }
    else if ( ! syms->isNil() ) {
        env->put(syms, vals);
    }
}
