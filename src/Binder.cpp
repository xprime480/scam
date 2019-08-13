#include "Binder.hpp"

#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

Binder::Binder(Env * capture)
    : capture(capture)
{
}

ScamValue
Binder::bind(ScamValue formals, ScamValue rest, ScamValue actuals) const
{
    Env * extended = capture->extend();

    ScamValue test = bindOne(extended, formals, rest, actuals);
    if ( isError(test) ) {
        return test;
    }

    ScamValue temp = makeEnv(extended);
    return temp;
}

ScamValue Binder::bindOne(Env * env,
                          ScamValue syms,
                          ScamValue rest,
                          ScamValue vals) const
{
    if ( isPair(syms) ) {
        ScamValue key = getCar(syms);
        ScamValue test = env->put(key, getCar(vals));
        if ( isError(test) ) {
            return test;
        }
        return bindOne(env, getCdr(syms), rest, getCdr(vals));
    }
    else if ( isSymbol(rest) ) {
        ScamValue test = env->put(rest, vals);
        if ( isError(test) ) {
            return test;
        }
    }

    return makeNothing();
}
