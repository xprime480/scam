#include "expr/ScamInstance.hpp"

#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/InstanceCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

namespace
{
    static ScamExpr * const self   =
        ExpressionFactory::makeSymbol("self", false);
    static ScamExpr * const parent =
        ExpressionFactory::makeSymbol("parent", false);
    static ScamExpr * const nil    = ExpressionFactory::makeNil();
}

ScamInstance::ScamInstance(ScamExpr * vars, ScamExpr * funs, Env * env)
    : priv(standardMemoryManager.make<Env>())
    , local(env->extend())
{
    size_t var_count = vars->length();
    for ( size_t n = 0 ; n < var_count ; ++n ) {
        ScamExpr * var = vars->nthcar(n);
        local->put(var, nil);
    }

    size_t fun_count = funs->length();
    for ( size_t n = 0 ; n < fun_count ; ++n ) {
        ScamExpr * fun = funs->nthcar(n);
        ScamExpr * name = fun->nthcar(0);
        ScamExpr * formals = fun->nthcar(1);
        ScamExpr * forms   = fun->nthcdr(1);
        ScamExpr * impl
            = ExpressionFactory::makeClosure(formals, forms, local, false);

        priv->put(name, impl);
    }
}

ScamInstance * ScamInstance::makeInstance(ScamExpr * vars,
                                          ScamExpr * funs,
                                          Env * env)
{
    return new ScamInstance(vars, funs, env);
}

void ScamInstance::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        local->mark();
        priv->mark();
    }
}

string ScamInstance::toString() const
{
    return "instance";
}

bool ScamInstance::hasApply() const
{
    return true;
}

void ScamInstance::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    ScamExpr * name = args->nthcar(0);
    ScamExpr * funargs = args->nthcdr(0);

    Continuation * newCont =
        standardMemoryManager.make<InstanceCont>(this, name, cont);
    funargs->mapEval(newCont, env);
}

bool ScamInstance::isProcedure() const
{
    return true;
}

bool ScamInstance::isInstance() const
{
    return true;
}

void ScamInstance::setSelf(ScamExpr * expr) const
{
    local->put(self, expr);
}

void ScamInstance::setParent(ScamExpr * expr) const
{
    local->put(parent, expr);
}
