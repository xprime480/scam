#include "expr/ClassCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ClassInitWorker.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ClassCont::ClassCont(ScamExpr * cls, Continuation * cont)
    : Continuation("ClassCont")
    , cls(cls)
    , cont(cont)
{
    ScamClassAdapter adapter(cls);
    env = adapter.getCapture();
}

ClassCont * ClassCont::makeInstance(ScamExpr * cls, Continuation * cont)
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

void ClassCont::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else {
        vector<ScamExpr *> instances;
        ScamExpr * result;

        result = build_instances(cls, instances);
        if ( result->error() ) {
            cont->run(result);
        }
        else {
            ScamExpr * instance = connect_instances(instances);
            init_instance(instance, expr);
        }
    }
}

ScamExpr *
ClassCont::build_instances(ScamExpr * cls,
                           vector<ScamExpr *> & instances) const
{
    ScamExpr * temp = cls;

    while ( cls->isClass() ) {
        ScamClassAdapter adapter(cls);
        ScamExpr * instance
            = ExpressionFactory::makeInstance(adapter.getVars(),
                                              adapter.getFuns(),
                                              env);
        instances.push_back(instance);
        temp = get_parent(cls);
        cls = temp;
    }

    if ( temp->error() ) {
        return temp;
    }

    if ( instances.empty() ) {
        return no_class_found(cls);
    }

    return ExpressionFactory::makeNil();
}

ScamExpr * ClassCont::connect_instances(vector<ScamExpr *> & instances) const
{
    ScamExpr * self = instances[0];
    for ( auto instance : instances ) {
        instance->setSelf(self);
    }

    size_t len = instances.size();
    for ( size_t idx = 0 ; idx < (len - 1) ; ++idx ) {
        ScamExpr * child = instances[idx];
        ScamExpr * parent = instances[idx+1];
        child->setParent(parent);
    }

    return self;
}

ScamExpr * ClassCont::get_parent(ScamClassAdapter const & adapter) const
{
    ScamExpr * baseName = adapter.getBase();
    if ( baseName->isNil() || baseName->toString() == "Root" ) {
        return ExpressionFactory::makeNil();
    }

    if ( ! env->check(baseName) ) {
        return base_class_not_found(baseName);
    }

    ScamExpr * b = env->get(baseName);
    if ( ! b->isClass() ) {
        return base_class_not_class(baseName, b);
    }

    return b;
}

ScamExpr * ClassCont::no_class_found(ScamExpr * cls) const
{
    stringstream s;
    s << "ScamClass could not build an instance from: "
      << cls->toString();
    return ExpressionFactory::makeError(s.str());
}

ScamExpr * ClassCont::base_class_not_found(ScamExpr * name) const
{
    stringstream s;
    s << "Class definition: " << name->toString() << " not found";
    return ExpressionFactory::makeError(s.str());
}

ScamExpr * ClassCont::base_class_not_class(ScamExpr * name,
                                           ScamExpr * value) const
{
    stringstream s;
    s << "Name: " << name->toString()
      << " is not a class; got: " << value->toString();
    return ExpressionFactory::makeError(s.str());
}

void ClassCont::init_instance(ScamExpr * instance, ScamExpr * expr) const
{
    workQueueHelper<ClassInitWorker>(instance, expr, cont, env);
}
