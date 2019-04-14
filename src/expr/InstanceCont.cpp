#include "expr/InstanceCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

InstanceCont::InstanceCont(ScamExpr * obj,
                           ScamExpr * name,
                           Continuation * cont)
    : Continuation("InstanceCont")
    , obj(obj)
    , name(name)
    , cont(cont)
{
}

InstanceCont * InstanceCont::makeInstance(ScamExpr * obj,
                                          ScamExpr * name,
                                          Continuation * cont)
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

void InstanceCont::run(ScamExpr * expr)
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
    Env * env = adapter.getEnv();
    func->apply(expr, cont, env);
}

ScamExpr * InstanceCont::find_func(ScamExpr * o) const
{
    ScamExpr * temp = o;

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

ScamExpr * InstanceCont::function_not_found() const
{
    ScamExpr * err =
        ExpressionFactory::makeError("Instance method ",
                                     name->toString(),
                                     " not found");
    cont->run(err);
    return ExpressionFactory::makeNil();
}
