
#include "expr/ScamClass.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamClassAdapter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static ScamExpr * const initSym =
        ExpressionFactory::makeSymbol("init", false);
    static ScamExpr * const nil     = ExpressionFactory::makeNil();

    extern void do_apply(ScamExpr * cls,
                         ScamExpr * args,
                         Continuation * cont,
                         Env env);
}

ScamClass::ScamClass(ScamExpr * base,
                     ScamExpr * vars,
                     ScamExpr * funs,
                     Env capture)
    : base(base)
    , vars(vars)
    , funs(funs)
    , capture(capture)
{
}

ScamClass * ScamClass::makeInstance(ScamExpr * base,
                                    ScamExpr * vars,
                                    ScamExpr * funs,
                                    Env capture)
{
    return new ScamClass(base, vars, funs, capture);
}

void ScamClass::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        if ( base ) {
            base->mark();
        }
        if ( vars ) {
            vars->mark();
        }
        if ( funs ) {
            funs->mark();
        }
    }
}

string ScamClass::toString() const
{
    return "class";
}

bool ScamClass::hasApply() const
{
    return true;
}

void ScamClass::apply(ScamExpr * args, Continuation * cont, Env env)
{
    do_apply(this, args, cont, env);
}

bool ScamClass::isProcedure() const
{
    return true;
}

bool ScamClass::isClass() const
{
    return true;
}

namespace
{
    class ClassInitCont: public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ClassInitCont(ScamExpr * instance, Continuation * cont)
            : Continuation("ClassInit")
            , instance(instance)
            , cont(cont)
        {
        }

        static ClassInitCont * makeInstance(ScamExpr * instance,
                                            Continuation * cont)
        {
            return new ClassInitCont(instance, cont);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
              Continuation::mark();
              instance->mark();
              cont->mark();
            }
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);
            cont->run(instance);
        }

    private:
        ScamExpr * instance;
        Continuation * cont;
    };

    class ClassInitWorker : public Worker
    {
    public:
        ClassInitWorker(ScamExpr * instance,
                        ScamExpr * args,
                        Continuation * cont,
                        Env env)
            : Worker("ClassInit")
            , instance(instance)
            , args(args)
            , cont(cont)
            , env(env)
        {
        }

        void run() override
        {
            Worker::run();

            Continuation * newCont
                = standardMemoryManager.make<ClassInitCont>(instance, cont);
            ScamExpr * newArgs
                = ExpressionFactory::makeCons(initSym, args);

            instance->apply(newArgs, newCont, env);
        }

    private:
        ScamExpr * instance;
        ScamExpr * args;
        Continuation * cont;
        Env        env;
    };

    class ClassCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ClassCont(ScamExpr * cls, Continuation * cont)
            : Continuation("ClassCont")
            , cls(cls)
            , cont(cont)
        {
            ScamClassAdapter adapter(cls);
            env = adapter.getCapture();
        }

        static ClassCont * makeInstance(ScamExpr * cls, Continuation * cont)
        {
            return new ClassCont(cls, cont);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                cls->mark();
                cont->mark();
            }
        }

        void run(ScamExpr * expr) override
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

    private:
        ScamExpr * cls;
        Continuation * cont;
        Env        env;

        ScamExpr *
        build_instances(ScamExpr * cls, vector<ScamExpr *> & instances) const
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

            return nil;
        }

        ScamExpr * connect_instances(vector<ScamExpr *> & instances) const
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

        ScamExpr * get_parent(ScamClassAdapter const & adapter) const
        {
            ScamExpr * baseName = adapter.getBase();
            if ( baseName->isNil() || baseName->toString() == "Root" ) {
                return nil;
            }

            if ( ! env.check(baseName) ) {
                return base_class_not_found(baseName);
            }

            ScamExpr * b = env.get(baseName);
            if ( ! b->isClass() ) {
                return base_class_not_class(baseName, b);
            }

            return b;
        }

        ScamExpr * no_class_found(ScamExpr * cls) const
        {
            stringstream s;
            s << "ScamClass could not build an instance from: "
              << cls->toString();
            return ExpressionFactory::makeError(s.str());
        }

        ScamExpr * base_class_not_found(ScamExpr * name) const
        {
            stringstream s;
            s << "Class definition: " << name->toString() << " not found";
            return ExpressionFactory::makeError(s.str());
        }

        ScamExpr * base_class_not_class(ScamExpr * name, ScamExpr * value) const
        {
            stringstream s;
            s << "Name: " << name->toString()
              << " is not a class; got: " << value->toString();
            return ExpressionFactory::makeError(s.str());
        }

        void init_instance(ScamExpr * instance, ScamExpr * expr) const
        {
            workQueueHelper<ClassInitWorker>(instance, expr, cont, env);
        }
    };

    class ClassWorker : public Worker
    {
    public:
        ClassWorker(ScamExpr * cls,
                    ScamExpr * args,
                    Continuation * cont,
                    Env env)
            : Worker("ClassWorker")
            , cls(cls)
            , args(args)
            , cont(cont)
            , env(env)
        {
        }

        void run() override
        {
            Worker::run();
            Continuation * newCont =
                standardMemoryManager.make<ClassCont>(cls, cont);
            args->mapEval(newCont, env);
        }

    private:
        ScamExpr * cls;
        ScamExpr * args;
        Continuation * cont;
        Env        env;
    };

    void do_apply(ScamExpr * cls, ScamExpr * args, Continuation * cont, Env env)
    {
        workQueueHelper<ClassWorker>(cls, args, cont, env);
    }
}
