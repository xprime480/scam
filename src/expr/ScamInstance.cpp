
#include "expr/ScamInstance.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static const ExprHandle self = ExpressionFactory::makeSymbol("self");
    static const ExprHandle nil = ExpressionFactory::makeNil();

    extern void do_apply(Env funs, ScamExpr * args, ContHandle cont, Env env);
}

ScamInstance::ScamInstance(ScamExpr * base,
                           ScamExpr * vars,
                           ScamExpr * funs,
                           Env env)
    : base(base->clone())
    , local(env.extend())
{
    size_t var_count = vars->length();
    for ( size_t n = 0 ; n < var_count ; ++n ) {
        ExprHandle var = vars->nthcar(n);
        local.put(var.get(), nil.get());
    }

    size_t fun_count = funs->length();
    for ( size_t n = 0 ; n < fun_count ; ++n ) {
        ExprHandle fun = funs->nthcar(n);
        ExprHandle name = fun->nthcar(0);
        ExprHandle formals = fun->nthcar(1);
        ExprHandle forms   = fun->nthcdr(1);
        ExprHandle impl
            = ExpressionFactory::makeClosure(formals.get(),
                                             forms.get(),
                                             local,
                                             false);
        priv.put(name.get(), impl.get());
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

void ScamInstance::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(priv, args, cont, env);
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
    local.put(self.get(), expr);
}

namespace
{
    bool find_func(Env priv, ScamExpr * func, ContHandle cont)
    {
        if ( ! priv.check(func) ) {
            stringstream s;
            s << "Instance method " << func->toString() << " not found";
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
            return false;
        }

        return true;
    }

    void do_apply(Env priv, ScamExpr * args, ContHandle cont, Env env)
    {
        ScamExpr * name = args->nthcar(0).get();
        if ( ! find_func(priv, name, cont) ) {
            return;
        }

        ScamExpr * func = priv.get(name).get();
        ScamExpr * farg = args->nthcdr(0).get();
        func->apply(farg, cont, env);
    }
}
