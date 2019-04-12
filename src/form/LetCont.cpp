#include "form/LetCont.hpp"

#include "Binder.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

LetCont::LetCont(ScamExpr * formals,
                 ScamExpr * forms,
                 Continuation * cont,
                 Env * env,
                 bool rebind)
    : LetCommonCont("Let", forms, cont)
    , formals(formals)
    , env(env)
    , rebind(rebind)
{
}

LetCont * LetCont::makeInstance(ScamExpr * formals,
                                ScamExpr * forms,
                                Continuation * cont,
                                Env * env,
                                bool rebind)
{
    return new LetCont(formals, forms, cont, env, rebind);
}

void LetCont::mark() const
{
    if ( ! isMarked() ) {
        LetCommonCont::mark();
        formals->mark();
        env->mark();
    }
}

void LetCont::do_let(ScamExpr * expr)
{
    Binder binder(env);
    ScamExpr * ff = formals;
    Env * extended = binder.bind(ff, expr);

    rebind_procs(extended);
    final_eval(extended);
}

void LetCont::rebind_procs(Env * extended)
{
    if ( ! rebind ) {
        return;
    }

    const size_t len = formals->length();
    for ( size_t n = 0 ; n < len ; ++n ) {
        ScamExpr * k = formals->nthcar(n);
        ScamExpr * v = extended->get(k);
        if ( v->isProcedure() ) {
            ScamExpr * newV = v->withEnvUpdate(extended);
            extended->assign(k, newV);
        }
    }
}
