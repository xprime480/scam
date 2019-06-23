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

Env * Binder::bind(ScamValue formals, ScamValue rest, ScamValue actuals) const
{
    Env * extended = capture->extend();
    bindOne(extended, formals, rest, actuals);
    return extended;
}

void Binder::bindOne(Env * env,
                     ScamValue syms,
                     ScamValue rest,
                     ScamValue vals) const
{
    if ( isPair(syms) ) {
        ScamValue key = getCar(syms);
        env->put(key, getCar(vals));
        bindOne(env, getCdr(syms), rest, getCdr(vals));
    }
    else if ( isSymbol(rest) ) {
        env->put(rest, vals);
    }
}
