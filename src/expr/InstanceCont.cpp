#include "expr/InstanceCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

InstanceCont::InstanceCont(ExprHandle obj,
                           ScamEnvKeyType name,
                           Continuation * cont)
    : Continuation("InstanceCont")
    , obj(obj)
    , name(name)
    , cont(cont)
{
}

InstanceCont * InstanceCont::makeInstance(ExprHandle obj,
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

void InstanceCont::run(ExprHandle expr)
{
    Continuation::run(expr);

    if ( TypePredicates::error(expr) ) {
        cont->run(expr);
        return;
    }

    ExprHandle func = find_func(obj);
    if ( TypePredicates::isNil(func) ) {
        return;
    }

    ScamInstanceAdapter adapter(obj);
    Env * env = adapter.getEnv();
    func->apply(expr, cont, env);
}

ExprHandle InstanceCont::find_func(ExprHandle o) const
{
    while ( TypePredicates::isInstance(o) ) {
        ScamInstanceAdapter adapter(o);
        Env * env = adapter.getFunctionMap();
        if ( env->check(name) ) {
            return env->get(name);
        }

        o = adapter.getParent();
    }

    return function_not_found();
}

ExprHandle InstanceCont::function_not_found() const
{
    ExprHandle err =
        ExpressionFactory::makeError("Instance method ",
                                     ExprWriter::write(name),
                                     " not found");
    cont->run(err);
    return ExpressionFactory::makeNil();
}
