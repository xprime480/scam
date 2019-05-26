#include "expr/InstanceCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

using namespace scam;
using namespace std;

InstanceCont::InstanceCont(ScamValue obj,
                           ScamEnvKeyType name,
                           Continuation * cont)
    : Continuation("InstanceCont")
    , obj(obj)
    , name(name)
    , cont(cont)
{
}

InstanceCont * InstanceCont::makeInstance(ScamValue obj,
                                          ScamEnvKeyType name,
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

void InstanceCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( error(expr) ) {
        cont->run(expr);
        return;
    }

    ScamValue func = find_func(obj);
    if ( isNil(func) ) {
        return;
    }

    ScamInstanceAdapter adapter(obj);
    Env * env = adapter.getEnv();
    apply(func, expr, cont, env);
}

ScamValue InstanceCont::find_func(ScamValue o) const
{
    while ( isInstance(o) ) {
        ScamInstanceAdapter adapter(o);
        Env * env = adapter.getFunctionMap();
        if ( env->check(name) ) {
            return env->get(name);
        }

        o = adapter.getParent();
    }

    return function_not_found();
}

ScamValue InstanceCont::function_not_found() const
{
    ScamValue err =
        ExpressionFactory::makeError("Instance method ",
                                     writeValue(name),
                                     " not found");
    cont->run(err);
    return ExpressionFactory::makeNil();
}
