#include "Binder.hpp"

#include "Env.hpp"
#include "expr/ScamData.hpp"
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
    if ( isPair(syms) ) {
        ScamValue key = getCar(syms);
        env->put(key, getCar(vals));
        bindOne(env, getCdr(syms), getCdr(vals));
    }
    else if ( ! isNull(syms) ) {
        env->put(syms, vals);
    }
}
