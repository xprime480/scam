#include "expr/InstanceCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ClassOps.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

using namespace scam;
using namespace std;

InstanceCont::InstanceCont(ScamValue obj,
                           ScamValue name,
                           Continuation * cont,
                           ScamEngine * engine)
    : Continuation("InstanceCont", engine)
    , obj(obj)
    , name(name)
    , cont(cont)
{
}

InstanceCont * InstanceCont::makeInstance(ScamValue obj,
                                          ScamValue name,
                                          Continuation * cont,
                                          ScamEngine * engine)
{
    return new InstanceCont(obj, name, cont, engine);
}

void InstanceCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        obj->mark();
        name->mark();
        cont->mark();
    }
}

void InstanceCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isError(value) ) {
        engine->handleError(value);
        return;
    }

    ScamValue func = find_func(obj);
    if ( isNull(func) ) {
        return;
    }

    Env * env = getInstanceEnv(obj);
    apply(func, value, cont, env, engine);
}

ScamValue InstanceCont::find_func(ScamValue o) const
{
    while ( isInstance(o) ) {
        Env * env = getInstanceFunctionMap(o);
        if ( env->check(name) ) {
            return env->get(name);
        }

        o = getInstanceParent(o);
    }

    return function_not_found();
}

ScamValue InstanceCont::function_not_found() const
{
    ScamValue err =
        makeErrorExtended("Instance method ", writeValue(name), " not found");
    engine->handleError(err);
    return makeNull();
}
