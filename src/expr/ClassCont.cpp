#include "expr/ClassCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ClassInitWorker.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamClass.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"

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

void ClassCont::run(ExprHandle expr)
{
    Continuation::run(expr);

    if ( TypePredicates::error(expr) ) {
        cont->run(expr);
    }
    else {
        InstanceVec instances;
        ExprHandle  result;

        result = build(cls, instances);
        if ( TypePredicates::error(result) ) {
            cont->run(result);
        }
        else {
            ScamInstance * instance = connect(instances);
            init(instance, expr);
        }
    }
}

ExprHandle ClassCont::build(ClassHandle cls, InstanceVec & instances) const
{
    ExprHandle temp;

    for ( ;; ) {
        ScamInstance * instance = ExpressionFactory::makeInstance(cls, env);
        instances.push_back(instance);

        temp = get_parent(cls);
        if ( ! TypePredicates::isClass(temp) ) {
            break;
        }

        cls = dynamic_cast<const ScamClass *>(temp);
    }

    if ( TypePredicates::error(temp) ) {
        return temp;
    }

    return ExpressionFactory::makeNil();
}

ScamInstance * ClassCont::connect(InstanceVec & instances) const
{
    ScamInstance * self = instances[0];
    for ( auto instance : instances ) {
        instance->setSelf(self);
    }

    size_t len = instances.size();
    for ( size_t idx = 0 ; idx < (len - 1) ; ++idx ) {
        ScamInstance * child = instances[idx];
        ScamInstance * parent = instances[idx+1];
        child->setParent(parent);
    }

    return self;
}

ExprHandle ClassCont::get_parent(ScamClassAdapter const & adapter) const
{
    ScamEnvKeyType base = adapter.getBase();
    if ( ExprWriter::write(base) == "Root" ) {
        return ExpressionFactory::makeNil();
    }

    if ( ! env->check(base) ) {
        return base_class_not_found(base);
    }

    ExprHandle b = env->get(base);
    if ( ! TypePredicates::isClass(b) ) {
        return base_class_not_class(base, b);
    }

    return b;
}

ExprHandle ClassCont::base_class_not_found(ScamEnvKeyType name) const
{
    stringstream s;
    s << "Class definition: " << ExprWriter::write(name) << " not found";
    return ExpressionFactory::makeError(s.str());
}

ExprHandle ClassCont::base_class_not_class(ScamEnvKeyType name,
                                           ExprHandle value) const
{
    stringstream s;
    s << "Name: " << ExprWriter::write(name)
      << " is not a class; got: " << ExprWriter::write(value);
    return ExpressionFactory::makeError(s.str());
}

void ClassCont::init(ScamInstance * instance, ExprHandle expr) const
{
    workQueueHelper<ClassInitWorker>(instance, expr, cont, env);
}
