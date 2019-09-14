#include "expr/ClassCont.hpp"

#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/ClassInitWorker.hpp"
#include "expr/ClassOps.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

using namespace scam;
using namespace std;

ClassCont::ClassCont(ScamValue cls, Continuation * cont)
    : Continuation("ClassCont")
    , cls(cls)
    , cont(cont)
{
    env = getClassCapture(cls);
}

ClassCont * ClassCont::makeInstance(ScamValue cls, Continuation * cont)
{
    return new ClassCont(cls, cont);
}

void ClassCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cls->mark();
        cont->mark();
        env->mark();
    }
}

void ClassCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        InstanceVec instances;
        ScamValue  result;

        result = build(cls, instances);
        if ( isUnhandledError(result) ) {
            ScamEngine::getEngine().handleError(value);
        }
        else {
            ScamValue instance = connect(instances);
            if ( isError(instance) ) {
                ScamEngine::getEngine().handleError(instance);
            }
            else {
                init(instance, value);
            }
        }
    }
}

ScamValue ClassCont::build(ScamValue cls, InstanceVec & instances) const
{
    ScamValue temp;

    for ( ;; ) {
        ScamValue instance = makeClassInstance(cls, env);
        if ( isUnhandledError(instance) ) {
            return instance;
        }

        instances.push_back(instance);

        temp = get_parent(cls);
        if ( ! isClass(temp) ) {
            break;
        }
        cls = temp;
    }

    if ( isError(temp) ) {
        return temp;
    }

    return makeNull();
}

ScamValue ClassCont::connect(InstanceVec & instances) const
{
    ScamValue self = instances[0];
    for ( auto instance : instances ) {
        ScamValue test = setInstanceSelf(instance, self);
        if ( isUnhandledError(test) ) {
            return test;
        }
    }

    size_t len = instances.size();
    for ( size_t idx = 0 ; idx < (len - 1) ; ++idx ) {
        ScamValue child = instances[idx];
        ScamValue parent = instances[idx+1];
        ScamValue test = setInstanceParent(child, parent);
        if ( isUnhandledError(test) ) {
            return test;
        }
    }

    return self;
}

ScamValue ClassCont::get_parent(ScamValue value) const
{
    ScamValue base = getClassBase(value);
    if ( writeValue(base) == "Root" ) {
        return makeNull();
    }

    ScamValue test = env->check(base);
    if ( isError(test) ) {
        return test;
    }
    else if ( ! truth(test) ) {
        return base_class_not_found(base);
    }

    ScamValue b = env->get(base);
    if ( ! isClass(b) ) {
        return base_class_not_class(base, b);
    }

    return b;
}

ScamValue ClassCont::base_class_not_found(ScamValue name) const
{
    return makeError("Class definition not found", name);
}

ScamValue ClassCont::base_class_not_class(ScamValue name, ScamValue value) const
{
    return makeError("Not a class", name);
}

void ClassCont::init(ScamValue instance, ScamValue expr) const
{
    workQueueHelper<ClassInitWorker>(instance, expr, cont, env);
}
