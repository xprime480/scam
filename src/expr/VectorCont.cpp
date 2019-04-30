#include "expr/VectorCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/VectorWorker.hpp"

using namespace scam;
using namespace std;

VectorCont::VectorCont(ExprVec const & forms,
                       ExprVec const & evaled,
                       Continuation * original,
                       Env * env)
    : Continuation("Vector")
    , forms(forms)
    , evaled(evaled)
    , original(original)
    , env(env)
{
}

VectorCont * VectorCont::makeInstance(ExprVec const & forms,
                                      ExprVec const & evaled,
                                      Continuation * original,
                                      Env * env)
{
    return new VectorCont(forms, evaled, original, env);
}

void VectorCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        for ( auto e : forms ) {
            e->mark();
        }
        for ( auto e : evaled ) {
            e->mark();
        }
        original->mark();
        env->mark();
    }
}

void VectorCont::run(ExprHandle expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        original->run(expr);
    }
    else {
        ExprVec e2(evaled);
        e2.push_back(expr);
        workQueueHelper<VectorWorker>(original, env, forms, e2);
    }
}
