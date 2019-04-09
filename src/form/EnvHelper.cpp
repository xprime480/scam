
#include "form/EnvHelper.hpp"

#include "Backtracker.hpp"
#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void apply_assign(ScamExpr * args,
                             ContHandle cont,
                             Env env,
                             ScamEngine * engine);
    extern void apply_define(ScamExpr * args,
                             ContHandle cont,
                             Env env,
                             ScamEngine * engine);
    extern void apply_undefine(ScamExpr * args,
                               ContHandle cont,
                               Env env,
                               ScamEngine * engine);
}

EnvHelper::EnvHelper(char const * name, ScamEngine * engine)
    : SpecialForm(name, true)
    , engine(engine)
{
}

Assign::Assign(ScamEngine * engine)
    : EnvHelper("assign!", engine)
{
}

Assign * Assign::makeInstance(ScamEngine * engine)
{
    return new Assign(engine);
}

void Assign::apply(ScamExpr * args, ContHandle cont, Env env)
{
    apply_assign(args, cont, env, engine);
}

Define::Define(ScamEngine * engine)
    : EnvHelper("define", engine)
{
}

Define * Define::makeInstance(ScamEngine * engine)
{
    return new Define(engine);
}

void Define::apply(ScamExpr * args, ContHandle cont, Env env)
{
    apply_define(args, cont, env, engine);
}

Undefine::Undefine(ScamEngine * engine)
  : EnvHelper("undefine", engine)
{
}

Undefine * Undefine::makeInstance(ScamEngine * engine)
{
    return new Undefine(engine);
}

void Undefine::apply(ScamExpr * args, ContHandle cont, Env env)
{
    apply_undefine(args, cont, env, engine);
}

namespace
{
    bool checkArgs(ScamExpr * args, ContHandle cont, bool exprNeeded)
    {
        const size_t expected = 1u + (exprNeeded ? 1u : 0u);
        const size_t actual   = args->length();

        if ( expected != actual ) {
            stringstream s;
            s << "Expecting " << expected << "forms for argument list; ";
            s << "got: " << args->toString();
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
            return false;
        }

        return true;
    }

    class EnvHelperWorker : public Worker
    {
    public:
        EnvHelperWorker(ScamExpr * args,
                        ContHandle cont,
                        Env env,
                        char const * name);
        void run() override;

    protected:
        ContHandle cont;
        Env env;

        virtual ContHandle getCont(ScamExpr * sym) const = 0;

    private:
        ScamExpr * args;
    };

    class AssignWorker : public EnvHelperWorker
    {
    public:
        AssignWorker(ScamExpr * args,
                     ContHandle cont,
                     Env env,
                     ScamEngine * engine);

    protected:
        ContHandle getCont(ScamExpr * sym) const override;

    private:
        ScamEngine * engine;
    };

    class DefineWorker : public EnvHelperWorker
    {
    public:
        DefineWorker(ScamExpr * args,
                     ContHandle cont,
                     Env env,
                     ScamEngine * engine);

    protected:
        ContHandle getCont(ScamExpr * sym) const override;

    private:
        ScamEngine * engine;
    };

    class UndefineWorker : public EnvHelperWorker
    {
    public:
        UndefineWorker(ScamExpr * args,
                       ContHandle cont,
                       Env env,
                       ScamEngine * engine);

    protected:
        ContHandle getCont(ScamExpr * sym) const override;
    };

    class EnvHelperCont : public Continuation
    {
    public:
        EnvHelperCont(ScamExpr * sym,
                      ContHandle cont,
                      Env env,
                      char const * name)
            : Continuation(name)
            , sym(sym)
            , env(env)
            , cont(cont)
        {
        }

        void run(ScamExpr * expr) override
        {
            finish(expr);
            cont->run(sym);
        }

    protected:
        ScamExpr * sym;
        mutable Env env;

        virtual void finish(ScamExpr * expr) const = 0;

    private:
        ContHandle cont;
    };

    class AssignBacktracker : public Backtracker
    {
    public:
        AssignBacktracker(ScamExpr * sym,
                          ScamExpr * old,
                          Env env,
                          BacktrackHandle backtracker)
            : Backtracker("DefineBacktracker", backtracker)
            , sym(sym)
            , old(old)
            , env(env)
        {
        }

        void run() override
        {
            Backtracker::run();
            env.assign(sym, old);
            Continuation * cont
                = standardMemoryManager.make<Continuation>("Assign Backtrack");
            runParent(cont);
        }

    private:
        ScamExpr *      sym;
        ScamExpr *      old;
        Env             env;
    };

    class AssignCont : public EnvHelperCont
    {
    public:
        AssignCont(ScamExpr * sym,
                   ContHandle cont,
                   Env env,
                   ScamEngine * engine)
            : EnvHelperCont(sym, cont, env, "Assign")
            , engine(engine)
        {
        }

    protected:
        void finish(ScamExpr * expr) const override
        {
            if ( expr->error() && expr->hasMeta("amb-error") ) {
                return;
            }

            ScamExpr * old = env.get(sym);

            env.assign(sym, expr);

            BacktrackHandle backtracker = engine->getBacktracker();
            if ( nullptr == backtracker.get() ) {
                return;
            }

            shared_ptr<Backtracker> bt =
                make_shared<AssignBacktracker>(sym,
                                               old,
                                               env,
                                               backtracker);
            engine->setBacktracker(bt);
        }

    private:
        ScamEngine * engine;
    };

    class DefineBacktracker : public Backtracker
    {
    public:
        DefineBacktracker(ScamExpr * sym,
                          Env env,
                          BacktrackHandle backtracker)
            : Backtracker("DefineBacktracker", backtracker)
            , sym(sym)
            , env(env)
        {
        }

        void run() override
        {
            Backtracker::run();
            env.remove(sym);
            Continuation * cont
                = standardMemoryManager.make<Continuation>("Define Backtrack");
            runParent(cont);
        }

    private:
        ScamExpr *      sym;
        Env             env;
    };

    class DefineCont : public EnvHelperCont
    {
    public:
        DefineCont(ScamExpr * sym,
                   ContHandle cont,
                   Env env,
                   ScamEngine * engine)
            : EnvHelperCont(sym, cont, env, "Define")
            , engine(engine)
        {
        }

    protected:
        void finish(ScamExpr * expr) const override
        {
            if ( expr->error() && expr->hasMeta("amb-error") ) {
                return;
            }

            env.put(sym, expr);

            BacktrackHandle backtracker = engine->getBacktracker();
            if ( nullptr == backtracker.get() ) {
                return;
            }

            shared_ptr<Backtracker> bt =
                make_shared<DefineBacktracker>(sym, env, backtracker);
            engine->setBacktracker(bt);
        }

    private:
        ScamEngine * engine;
    };

    class UndefineCont : public EnvHelperCont
    {
    public:
        UndefineCont(ScamExpr * sym, ContHandle cont, Env env)
            : EnvHelperCont(sym, cont, env, "Undefine")
        {
        }

    protected:
        void finish(ScamExpr * expr) const override
        {
            env.remove(sym);
        }
    };

    EnvHelperWorker::EnvHelperWorker(ScamExpr * args,
                                     ContHandle cont,
                                     Env env,
                                     char const * name)
        : Worker(name)
        , cont(cont)
        , env(env)
        , args(args)
    {
    }

    void EnvHelperWorker::run()
    {
        Worker::run();

        ScamExpr * sym = args->getCar();
        ContHandle c = getCont(sym);
        if ( args->length() > 1 ) {
            ScamExpr * expr = args->nthcar(1);
            expr->eval(c, env);
        }
        else {
            ScamExpr * expr = ExpressionFactory::makeNil();
            c->run(expr);
        }
    }

    AssignWorker::AssignWorker(ScamExpr * args,
                               ContHandle cont,
                               Env env,
                               ScamEngine * engine)
        : EnvHelperWorker(args, cont, env, "Assign")
        , engine(engine)
    {
    }

    ContHandle AssignWorker::getCont(ScamExpr * sym) const
    {
        return make_shared<AssignCont>(sym, cont, env, engine);
    }

    DefineWorker::DefineWorker(ScamExpr * args,
                               ContHandle cont,
                               Env env,
                               ScamEngine * engine)
        : EnvHelperWorker(args, cont, env, "Define")
        , engine(engine)
    {
    }

    ContHandle DefineWorker::getCont(ScamExpr * sym) const
    {
        return make_shared<DefineCont>(sym, cont, env, engine);
    }

    UndefineWorker::UndefineWorker(ScamExpr * args,
                                   ContHandle cont,
                                   Env env,
                                   ScamEngine * engine)
        : EnvHelperWorker(args, cont, env, "Undefine")
    {
    }

    ContHandle UndefineWorker::getCont(ScamExpr * sym) const
    {
        return make_shared<UndefineCont>(sym, cont, env);
    }

    void apply_assign(ScamExpr * args,
                      ContHandle cont,
                      Env env,
                      ScamEngine * engine)
    {
        if ( checkArgs(args, cont, true) ) {
            workQueueHelper<AssignWorker>(args, cont, env, engine);
        }
    }

    void apply_define(ScamExpr * args,
                      ContHandle cont,
                      Env env,
                      ScamEngine * engine)
    {
        if ( checkArgs(args, cont, true) ) {
            workQueueHelper<DefineWorker>(args, cont, env, engine);
        }
    }

    void apply_undefine(ScamExpr * args,
                        ContHandle cont,
                        Env env,
                        ScamEngine * engine)
    {
        if ( checkArgs(args, cont, false) ) {
            workQueueHelper<UndefineWorker>(args, cont, env, engine);
        }
    }
}
