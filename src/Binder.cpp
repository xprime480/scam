#include "Binder.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ScamSymbol.hpp"
#include "expr/TypePredicates.hpp"

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
    if ( TypePredicates::isCons(syms) ) {
        ScamEnvKeyType key = dynamic_cast<ScamEnvKeyType>(syms->getCar());
        env->put(key, vals->getCar());
        bindOne(env, syms->getCdr(), vals->getCdr());
    }
    else if ( ! TypePredicates::isNil(syms) ) {
        ScamEnvKeyType key = dynamic_cast<ScamEnvKeyType>(syms);
        env->put(key, vals);
    }
}
