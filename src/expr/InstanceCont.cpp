#include "expr/InstanceCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

InstanceCont::InstanceCont(ExprHandle obj,
                           ExprHandle name,
                           Continuation * cont)
    : Continuation("InstanceCont")
    , obj(obj)
    , name(name)
    , cont(cont)
{
}

InstanceCont *
InstanceCont::makeInstance(ExprHandle obj, ExprHandle name, Continuation * cont)
{
    return new InstanceCont(obj, name, cont);
}

void InstanceCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        obj->mark();
        name->mark();
        cont->mark();
    }
}

void InstanceCont::run(ExprHandle expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
        return;
    }

    ExprHandle func = find_func(obj);
    if ( func->isNil() ) {
        return;
    }

    ScamInstanceAdapter adapter(obj);
    Env * env = adapter.getEnv();
    func->apply(expr, cont, env);
}

ExprHandle InstanceCont::find_func(ExprHandle o) const
{
    ExprHandle temp = o;

    while ( o->isInstance() ) {
        ScamInstanceAdapter adapter(o);
        Env * env = adapter.getFunctionMap();
        if ( env->check(name) ) {
            return env->get(name);
        }

        temp = adapter.getParent();
        o    = temp;
    }

    return function_not_found();
}

ExprHandle InstanceCont::function_not_found() const
{
    ExprHandle err =
        ExpressionFactory::makeError("Instance method ",
                                     name->toString(),
                                     " not found");
    cont->run(err);
    return ExpressionFactory::makeNil();
}
