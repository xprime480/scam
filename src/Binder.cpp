#include "Binder.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ScamSymbol.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

Binder::Binder(Env * capture)
    : capture(capture)
{
}

Env * Binder::bind(ScamValue formals, ScamValue actuals) const
{
    Env * extended = capture->extend();
    bindOne(extended, formals, actuals);
    return extended;
}

void Binder::bindOne(Env * env, ScamValue syms, ScamValue vals) const
{
    if ( isCons(syms) ) {
        ScamEnvKeyType key = dynamic_cast<ScamEnvKeyType>(getCar(syms));
        env->put(key, getCar(vals));
        bindOne(env, getCdr(syms), getCdr(vals));
    }
    else if ( ! isNil(syms) ) {
        ScamEnvKeyType key = dynamic_cast<ScamEnvKeyType>(syms);
        env->put(key, vals);
    }
}
