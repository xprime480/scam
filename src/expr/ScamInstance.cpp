#include "expr/ScamInstance.hpp"

#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/InstanceCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

namespace
{
    static ConstExprHandle self   =
        ExpressionFactory::makeSymbol("self", false);
    
    static ConstExprHandle parent =
        ExpressionFactory::makeSymbol("parent", false);
    
    static ExprHandle nil = ExpressionFactory::makeNil();
}

ScamInstance::ScamInstance(ExprHandle vars, ExprHandle funs, Env * env)
    : priv(standardMemoryManager.make<Env>())
    , local(env->extend())
{
    size_t var_count = vars->length();
    for ( size_t n = 0 ; n < var_count ; ++n ) {
        ExprHandle var = vars->nthcar(n);
        local->put(var, nil);
    }

    size_t fun_count = funs->length();
    for ( size_t n = 0 ; n < fun_count ; ++n ) {
        ExprHandle fun = funs->nthcar(n);
        ExprHandle name = fun->nthcar(0);
        ExprHandle formals = fun->nthcar(1);
        ExprHandle forms   = fun->nthcdr(1);
        ExprHandle impl
            = ExpressionFactory::makeClosure(formals, forms, local, false);

        priv->put(name, impl);
    }
}

ScamInstance * ScamInstance::makeInstance(ExprHandle vars,
                                          ExprHandle funs,
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

void ScamInstance::apply(ExprHandle args, Continuation * cont, Env * env)
{
    ExprHandle name = args->nthcar(0);
    ExprHandle funargs = args->nthcdr(0);

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

void ScamInstance::setSelf(ExprHandle expr) const
{
    local->put(self, expr);
}

void ScamInstance::setParent(ExprHandle expr) const
{
    local->put(parent, expr);
}
