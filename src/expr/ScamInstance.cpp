
#include "expr/ScamInstance.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamInstanceAdapter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static ScamExpr * const self   =
        ExpressionFactory::makeSymbol("self", false);
    static ScamExpr * const parent =
        ExpressionFactory::makeSymbol("parent", false);
    static ScamExpr * const nil    = ExpressionFactory::makeNil();

    extern void
    do_apply(ScamExpr * obj, ScamExpr * args, Continuation * cont, Env env);
}

ScamInstance::ScamInstance(ScamExpr * vars, ScamExpr * funs, Env env)
    : local(env.extend())
{
    size_t var_count = vars->length();
    for ( size_t n = 0 ; n < var_count ; ++n ) {
        ScamExpr * var = vars->nthcar(n);
        local.put(var, nil);
    }

    size_t fun_count = funs->length();
    for ( size_t n = 0 ; n < fun_count ; ++n ) {
        ScamExpr * fun = funs->nthcar(n);
        ScamExpr * name = fun->nthcar(0);
        ScamExpr * formals = fun->nthcar(1);
        ScamExpr * forms   = fun->nthcdr(1);
        ScamExpr * impl
            = ExpressionFactory::makeClosure(formals, forms, local, false);

        priv.put(name, impl);
    }
}

ScamInstance * ScamInstance::makeInstance(ScamExpr * vars,
                                          ScamExpr * funs,
                                          Env env)
{
    return new ScamInstance(vars, funs, env);
}

string ScamInstance::toString() const
{
    return "instance";
}

bool ScamInstance::hasApply() const
{
    return true;
}

void ScamInstance::apply(ScamExpr * args, Continuation * cont, Env env)
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
    local.put(self, expr);
}

void ScamInstance::setParent(ScamExpr * expr) const
{
    local.put(parent, expr);
}

namespace
{
    class InstanceCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        InstanceCont(ScamExpr * obj, ScamExpr * name, Continuation * cont)
            : Continuation("InstanceCont")
            , obj(obj)
            , name(name)
            , cont(cont)
        {
        }

        static InstanceCont *
        makeInstance(ScamExpr * obj, ScamExpr * name, Continuation * cont)
        {
            return new InstanceCont(obj, name, cont);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                obj->mark();
                name->mark();
                cont->mark();
            }
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);

            if ( expr->error() ) {
                cont->run(expr);
                return;
            }

            ScamExpr * func = find_func(obj);
            if ( func->isNil() ) {
                return;
            }

            ScamInstanceAdapter adapter(obj);
            Env env = adapter.getEnv();
            func->apply(expr, cont, env);
        }

    private:
        ScamExpr * obj;
        ScamExpr * name;
        Continuation * cont;

        ScamExpr * find_func(ScamExpr * o) const
        {
            ScamExpr * temp = o;

            while ( o->isInstance() ) {
                ScamInstanceAdapter adapter(o);
                Env env = adapter.getFunctionMap();
                if ( env.check(name) ) {
                    return env.get(name);
                }

                temp = adapter.getParent();
                o    = temp;
            }

            return function_not_found();
        }

        ScamExpr * function_not_found() const
        {
            stringstream s;
            s << "Instance method " << name->toString() << " not found";
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
            return nil;
        }
    };

    void do_apply(ScamExpr * obj, ScamExpr * args, Continuation * cont, Env env)
    {
        ScamExpr * name = args->nthcar(0);
        ScamExpr * funargs = args->nthcdr(0);

        Continuation * newCont =
            standardMemoryManager.make<InstanceCont>(obj, name, cont);
        funargs->mapEval(newCont, env);
    }
}
