#include "expr/VectorWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/VectorCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

VectorWorker::VectorWorker(Continuation * cont,
                           Env * env,
                           ExprVec const & forms)
    : Worker("Vector")
    , forms(forms)
    , original(cont)
    , env(env)
{
}

VectorWorker::VectorWorker(Continuation * cont,
                           Env * env,
                           ExprVec const & forms,
                           ExprVec const & evaled)
    : Worker("Vector 2")
    , forms(forms)
    , evaled(evaled)
    , original(cont)
    , env(env)
{
}

VectorWorker * VectorWorker::makeInstance(Continuation * cont,
                                          Env * env,
                                          ExprVec const & forms)
{
    return new VectorWorker(cont, env, forms);
}

VectorWorker * VectorWorker::makeInstance(Continuation * cont,
                                          Env * env,
                                          ExprVec const & forms,
                                          ExprVec const & evaled)
{
    return new VectorWorker(cont, env, forms, evaled);
}

void VectorWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        env->mark();
        original->mark();
        for ( const auto f : forms ) {
            f->mark();
        }
        for ( const auto e : evaled ) {
            e->mark();
        }
    }
}

void VectorWorker::run()
{
    Worker::run();

    if ( forms.size() == evaled.size() ) {
        ScamExpr * value = ExpressionFactory::makeVector(evaled);
        original->run(value);
    }
    else {
        size_t index = evaled.size();
        ScamExpr * expr = forms[index];
        Continuation * cont
            = standardMemoryManager.make<VectorCont>(forms,
                                                     evaled,
                                                     original,
                                                     env);
        expr->eval(cont, env);
    }
}

