
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
    static const ExprHandle initSym = ExpressionFactory::makeSymbol("init");
    static const ExprHandle nil     = ExpressionFactory::makeNil();

    extern void do_apply(ScamExpr * cls,
                         ScamExpr * args,
                         ContHandle cont,
                         Env env);
}

ScamClass::ScamClass(ScamExpr * base,
                     ScamExpr * vars,
                     ScamExpr * funs,
                     Env capture)
    : base(base->clone())
    , vars(vars->clone())
    , funs(funs->clone())
    , capture(capture)
{
}

string ScamClass::toString() const
{
    return "class";
}

bool ScamClass::hasApply() const
{
    return true;
}

void ScamClass::apply(ScamExpr * args, ContHandle cont, Env env)
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
    public:
        ClassInitCont(ScamExpr * instance, ContHandle cont)
            : Continuation("ClassInit")
            , instance(instance->clone())
            , cont(cont)
        {
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);
            cont->run(instance.get());
        }

    private:
        ExprHandle instance;
        ContHandle cont;
    };

    class ClassInitWorker : public Worker
    {
    public:
        ClassInitWorker(ScamExpr * instance,
                        ScamExpr * args,
                        ContHandle cont,
                        Env env)
            : Worker("ClassInit")
            , instance(instance->clone())
            , args(args->clone())
            , cont(cont)
            , env(env)
        {
        }

        void run() override
        {
            Worker::run();

            ContHandle newCont
                = make_shared<ClassInitCont>(instance.get(), cont);
            ExprHandle newArgs
                = ExpressionFactory::makeCons(initSym.get(), args.get());

            instance->apply(newArgs.get(), newCont, env);
        }

    private:
        ExprHandle instance;
        ExprHandle args;
        ContHandle cont;
        Env        env;
    };

    class ClassCont : public Continuation
    {
    public:
        ClassCont(ScamExpr * cls, ContHandle cont)
            : Continuation("ClassCont")
            , cls(cls->clone())
            , cont(cont)
        {
            ScamClassAdapter adapter(cls);
            env = adapter.getCapture();
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);

            if ( expr->error() ) {
                cont->run(expr);
            }
            else {
                vector<ExprHandle> instances;
                ExprHandle result;

                result = build_instances(cls.get(), instances);
                if ( result->error() ) {
                    cont->run(result.get());
                }
                else {
                    ExprHandle instance = connect_instances(instances);
                    init_instance(instance.get(), expr);
                }
            }
        }

    private:
        ExprHandle cls;
        ContHandle cont;
        Env        env;

        ExprHandle
        build_instances(ScamExpr * cls, vector<ExprHandle> & instances) const
        {
            ExprHandle temp = cls->clone();

            while ( cls->isClass() ) {
                ScamClassAdapter adapter(cls);
                ExprHandle instance
                    = ExpressionFactory::makeInstance(adapter.getVars(),
                                                      adapter.getFuns(),
                                                      env);
                instances.push_back(instance);
                temp = get_parent(cls);
                cls = temp.get();
            }

            if ( temp->error() ) {
                return temp;
            }

            if ( instances.empty() ) {
                return no_class_found(cls);
            }

            return nil;
        }

        ExprHandle connect_instances(vector<ExprHandle> & instances) const
        {
            ScamExpr * self = instances[0].get();
            for ( auto instance : instances ) {
                instance->setSelf(self);
            }

            size_t len = instances.size();
            for ( size_t idx = 0 ; idx < (len - 1) ; ++idx ) {
                ScamExpr * child = instances[idx].get();
                ScamExpr * parent = instances[idx+1].get();
                child->setParent(parent);
            }

            return self->clone();
        }

        ExprHandle get_parent(ScamClassAdapter const & adapter) const
        {
            ScamExpr * baseName = adapter.getBase();
            if ( baseName->isNil() || baseName->toString() == "Root" ) {
                return nil;
            }

            if ( ! env.check(baseName) ) {
                return base_class_not_found(baseName);
            }

            ExprHandle b = env.get(baseName);
            if ( ! b->isClass() ) {
                return base_class_not_class(baseName, b.get());
            }

            return b;
        }

        ExprHandle no_class_found(ScamExpr * cls) const
        {
            stringstream s;
            s << "ScamClass could not build an instance from: "
              << cls->toString();
            return ExpressionFactory::makeError(s.str());
        }

        ExprHandle base_class_not_found(ScamExpr * name) const
        {
            stringstream s;
            s << "Class definition: " << name->toString() << " not found";
            return ExpressionFactory::makeError(s.str());
        }

        ExprHandle base_class_not_class(ScamExpr * name, ScamExpr * value) const
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
        ClassWorker(ScamExpr * cls, ScamExpr * args, ContHandle cont, Env env)
            : Worker("ClassWorker")
            , cls(cls->clone())
            , args(args->clone())
            , cont(cont)
            , env(env)
        {
        }

        void run() override
        {
            Worker::run();
            ContHandle newCont = make_shared<ClassCont>(cls.get(), cont);
            args->mapEval(newCont, env);
        }

    private:
        ExprHandle cls;
        ExprHandle args;
        ContHandle cont;
        Env        env;
    };

    void do_apply(ScamExpr * cls, ScamExpr * args, ContHandle cont, Env env)
    {
        workQueueHelper<ClassWorker>(cls, args, cont, env);
    }
}
