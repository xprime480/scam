#include "form/LetCont.hpp"

#include "Binder.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

LetCont::LetCont(ScamValue formals,
                 ScamValue forms,
                 Continuation * cont,
                 Env * env,
                 ScamEngine * engine,
                 bool rebind)
    : LetCommonCont("Let", forms, cont, engine)
    , formals(formals)
    , env(env)
    , rebind(rebind)
{
}

LetCont * LetCont::makeInstance(ScamValue formals,
                                ScamValue forms,
                                Continuation * cont,
                                Env * env, ScamEngine * engine,
                                bool rebind)
{
    return new LetCont(formals, forms, cont, env, engine, rebind);
}

void LetCont::mark() const
{
    if ( ! isMarked() ) {
        LetCommonCont::mark();
        formals->mark();
        env->mark();
    }
}

void LetCont::do_let(ScamValue expr)
{
    Binder binder(env);
    ScamValue ff = formals;
    Env * extended = binder.bind(ff, expr);

    rebind_procs(extended);
    final_eval(extended);
}

void LetCont::rebind_procs(Env * extended)
{
    if ( ! rebind ) {
        return;
    }

    const size_t len = length(formals);
    for ( size_t n = 0 ; n < len ; ++n ) {
        ScamValue k = nthcar(formals, n);
        ScamValue v = extended->get(k);
        if ( isProcedure(v) ) {
            ScamValue newV = withEnvUpdate(v, extended);
            extended->assign(k, newV);
        }
    }
}
