#include "form/LetCont.hpp"

#include "Binder.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

LetCont::LetCont(ScamValue formals,
                 ScamValue forms,
                 Continuation * cont,
                 Env * env,
                 bool rebind)
    : LetCommonCont("Let", forms, cont)
    , formals(formals)
    , env(env)
    , rebind(rebind)
{
}

LetCont * LetCont::makeInstance(ScamValue formals,
                                ScamValue forms,
                                Continuation * cont,
                                Env * env,
                                bool rebind)
{
    return new LetCont(formals, forms, cont, env, rebind);
}

void LetCont::mark()
{
    if ( ! isMarked() ) {
        LetCommonCont::mark();
        formals->mark();
        env->mark();
    }
}

ScamValue LetCont::do_let(ScamValue expr)
{
    Binder binder(env);
    ScamValue ff = formals;

    ScamValue test = binder.bind(ff, makeNothing(), expr);
    if ( isError(test) ) {
        return test;
    }

    Env * extended = test->envValue();

    test = rebind_procs(extended);
    if ( isError(test) ) {
        return test;
    }

    final_eval(extended);

    return makeNothing();
}

ScamValue LetCont::rebind_procs(Env * extended)
{
    if ( ! rebind ) {
        return makeNothing();
    }

    const size_t len = length(formals);
    for ( size_t n = 0 ; n < len ; ++n ) {
        ScamValue k = nthcar(formals, n);
        ScamValue v = extended->get(k);
        if ( isUnhandledError(v) ) {
            return v;
        }
        else if ( isProcedure(v) ) {
            ScamValue newV = withEnvUpdate(v, extended);
            ScamValue test = extended->assign(k, newV);
            if ( isError(test) ) {
                return test;
            }
        }
    }

    return makeNothing();
}
