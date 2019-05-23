#include "expr/ClassCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ClassInitWorker.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamClass.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ClassCont::ClassCont(ClassHandle cls, Continuation * cont)
    : Continuation("ClassCont")
    , cls(cls)
    , cont(cont)
{
    ScamClassAdapter adapter(cls);
    env = adapter.getCapture();
}

ClassCont * ClassCont::makeInstance(ClassHandle cls, Continuation * cont)
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
            ScamInstance * instance = connect(instances);
            init(instance, expr);
        }
    }
}

ScamValue ClassCont::build(ClassHandle cls, InstanceVec & instances) const
{
    ScamValue temp;

    for ( ;; ) {
        ScamInstance * instance = ExpressionFactory::makeInstance(cls, env);
        instances.push_back(instance);

        temp = get_parent(cls);
        if ( ! isClass(temp) ) {
            break;
        }

        cls = dynamic_cast<const ScamClass *>(temp);
    }

    if ( error(temp) ) {
        return temp;
    }

    return ExpressionFactory::makeNil();
}

ScamInstance * ClassCont::connect(InstanceVec & instances) const
{
    ScamInstance * self = instances[0];
    for ( auto instance : instances ) {
        setSelf(instance, self);
    }

    size_t len = instances.size();
    for ( size_t idx = 0 ; idx < (len - 1) ; ++idx ) {
        ScamInstance * child = instances[idx];
        ScamInstance * parent = instances[idx+1];
        setParent(child, parent);
    }

    return self;
}

ScamValue ClassCont::get_parent(ScamClassAdapter const & adapter) const
{
    ScamEnvKeyType base = adapter.getBase();
    if ( writeValue(base) == "Root" ) {
        return ExpressionFactory::makeNil();
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

ScamValue ClassCont::base_class_not_found(ScamEnvKeyType name) const
{
    stringstream s;
    s << "Class definition: " << writeValue(name) << " not found";
    return ExpressionFactory::makeError(s.str());
}

ScamValue ClassCont::base_class_not_class(ScamEnvKeyType name,
                                           ScamValue value) const
{
    stringstream s;
    s << "Name: " << writeValue(name)
      << " is not a class; got: " << writeValue(value);
    return ExpressionFactory::makeError(s.str());
}

void ClassCont::init(ScamInstance * instance, ScamValue expr) const
{
    workQueueHelper<ClassInitWorker>(instance, expr, cont, env);
}
