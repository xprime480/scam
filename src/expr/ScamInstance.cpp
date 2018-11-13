
#include "expr/ScamInstance.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamInstanceAdapter.hpp"

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
    class InstanceCont : public Continuation
    {
    public:
        InstanceCont(ScamExpr * obj, ScamExpr * name, ContHandle cont)
            : Continuation("InstanceCont")
            , obj(obj->clone())
            , name(name->clone())
            , cont(cont)
        {
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);

            if ( expr->error() ) {
                cont->run(expr);
                return;
            }

            ExprHandle func = find_func(obj.get());
            if ( func->isNil() ) {
                return;
            }

            ScamInstanceAdapter adapter(obj.get());
            Env env = adapter.getEnv();
            func->apply(expr, cont, env);
        }

    private:
        ExprHandle obj;
        ExprHandle name;
        ContHandle cont;

        ExprHandle find_func(ScamExpr * o) const
        {
            ExprHandle temp = o->clone();

            while ( o->isInstance() ) {
                ScamInstanceAdapter adapter(o);
                Env env = adapter.getFunctionMap();
                if ( env.check(name.get()) ) {
                    return env.get(name.get());
                }

                temp = adapter.getParent();
                o    = temp.get();
            }

            return function_not_found();
        }

        ExprHandle function_not_found() const
        {
            stringstream s;
            s << "Instance method " << name->toString() << " not found";
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
            return nil;
        }
    };

    void do_apply(ScamExpr * obj, ScamExpr * args, ContHandle cont, Env env)
    {
        ScamExpr * name = args->nthcar(0).get();
        ScamExpr * funargs = args->nthcdr(0).get();

        ContHandle newCont = make_shared<InstanceCont>(obj, name, cont);
        funargs->mapEval(newCont, env);
    }

}
