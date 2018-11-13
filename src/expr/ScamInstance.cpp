
#include "expr/ScamInstance.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamInstanceAdapter.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static const ExprHandle self = ExpressionFactory::makeSymbol("self");
    static const ExprHandle parent = ExpressionFactory::makeSymbol("parent");
    static const ExprHandle nil = ExpressionFactory::makeNil();

    extern void
    do_apply(ScamExpr * obj, ScamExpr * args, ContHandle cont, Env env);
}

ScamInstance::ScamInstance(ScamExpr * vars, ScamExpr * funs, Env env)
    : local(env.extend())
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
    do_apply(this, args, cont, env);
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

void ScamInstance::setParent(ScamExpr * expr) const
{
    local.put(parent.get(), expr);
}

namespace
{
    ExprHandle function_not_found(ScamExpr * func, ContHandle cont)
    {
        stringstream s;
        s << "Instance method " << func->toString() << " not found";
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err.get());
        return nil;
    }

    ExprHandle find_func(ScamExpr * obj, ScamExpr * func, ContHandle cont)
    {
        ExprHandle temp = obj->clone();

        while ( obj->isInstance() ) {
            ScamInstanceAdapter adapter(obj);
            Env env = adapter.getFunctionMap();
            if ( env.check(func) ) {
                return env.get(func);
            }

            temp = adapter.getParent();
            obj  = temp.get();
        }

        return function_not_found(func, cont);
    }

    void do_apply(ScamExpr * obj, ScamExpr * args, ContHandle cont, Env env)
    {
        ScamExpr * name = args->nthcar(0).get();
        ExprHandle func = find_func(obj, name, cont);
        if ( func->isNil() ) {
            return;
        }

        ScamExpr * farg = args->nthcdr(0).get();
        func->apply(farg, cont, env);
    }
}
