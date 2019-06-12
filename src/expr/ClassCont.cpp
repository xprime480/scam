#include "expr/ClassCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ClassInitWorker.hpp"
#include "expr/ClassOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

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

void ClassCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cls->mark();
        cont->mark();
        env->mark();
    }
}

void ClassCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( error(expr) ) {
        cont->run(expr);
    }
    else {
        InstanceVec instances;
        ScamValue  result;

        result = build(cls, instances);
        if ( error(result) ) {
            cont->run(result);
        }
        else {
            ScamValue instance = connect(instances);
            init(instance, expr);
        }
    }
}

ScamValue ClassCont::build(ScamValue cls, InstanceVec & instances) const
{
    ScamValue temp;

    for ( ;; ) {
        ScamValue instance = makeClassInstance(cls, env);
        instances.push_back(instance);

        temp = get_parent(cls);
        if ( ! isClass(temp) ) {
            break;
        }
        cls = temp;
    }

    if ( error(temp) ) {
        return temp;
    }

    return makeNull();
}

ScamValue ClassCont::connect(InstanceVec & instances) const
{
    ScamValue self = instances[0];
    for ( auto instance : instances ) {
        setInstanceSelf(instance, self);
    }

    size_t len = instances.size();
    for ( size_t idx = 0 ; idx < (len - 1) ; ++idx ) {
        ScamValue child = instances[idx];
        ScamValue parent = instances[idx+1];
        setInstanceParent(child, parent);
    }

    return self;
}

ScamValue ClassCont::get_parent(ScamValue value) const
{
    ScamValue base = getClassBase(value);
    if ( writeValue(base) == "Root" ) {
        return makeNull();
    }

    if ( ! env->check(base) ) {
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
    stringstream s;
    s << "Class definition: " << writeValue(name) << " not found";
    return makeError(s.str());
}

ScamValue ClassCont::base_class_not_class(ScamValue name, ScamValue value) const
{
    stringstream s;
    s << "Name: " << writeValue(name)
      << " is not a class; got: " << writeValue(value);
    return makeError(s.str());
}

void ClassCont::init(ScamValue instance, ScamValue expr) const
{
    workQueueHelper<ClassInitWorker>(instance, expr, cont, env);
}
