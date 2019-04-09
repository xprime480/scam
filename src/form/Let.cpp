
#include "form/Let.hpp"

#include "Backtracker.hpp"
#include "Binder.hpp"
#include "Continuation.hpp"
#include "EvalWorker.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamListAdapter.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void let_impl(ScamExpr * args,
                         ContHandle cont,
                         Env env,
                         bool rebind);

    extern void letstar_impl(ScamExpr * args,
                             ContHandle cont,
                             Env env,
                             ScamEngine * engine);
}

Let::Let()
    : SpecialForm("let")
{
}

Let * Let::makeInstance()
{
    static Let instance;
    return &instance;
}

void Let::apply(ScamExpr * args, ContHandle cont, Env env)
{
    let_impl(args, cont, env, false);
}

LetStar::LetStar(ScamEngine * engine)
    : SpecialForm("let*", true)
    , engine(engine)
{
}

LetStar * LetStar::makeInstance(ScamEngine * engine)
{
    return new LetStar(engine);
}

void LetStar::apply(ScamExpr * args, ContHandle cont, Env env)
{
    letstar_impl(args, cont, env, engine);
}


LetRec::LetRec()
    : SpecialForm("letrec")
{
}

LetRec * LetRec::makeInstance()
{
    static LetRec instance;
    return &instance;
}

void LetRec::apply(ScamExpr * args, ContHandle cont, Env env)
{
    let_impl(args, cont, env, true);
}

namespace
{
    ScamExpr * safeCons(ScamExpr * expr)
    {
        if ( expr->isCons() ) {
            return expr;
        }
        return ExpressionFactory::makeList(expr);
    }

    class LetCommonCont : public Continuation
    {
    public:
        LetCommonCont(char const * name, ScamExpr * forms, ContHandle cont);

        void run(ScamExpr * expr) override;

    protected:
        ScamExpr * forms;
        ContHandle cont;

        virtual void do_let(ScamExpr * expr) = 0;
        void final_eval(Env env);
    };

    class LetCont : public LetCommonCont
    {
    public:
        LetCont(ScamExpr * formals,
                ScamExpr * forms,
                ContHandle cont,
                Env env,
                bool rebind);

    protected:
        void do_let(ScamExpr * expr) override;

    private:
        ScamExpr * formals;
        Env        env;
        bool       rebind;

        void rebind_procs(Env extended);
    };

    class LetStarBacktracker : public Backtracker
    {
    public:
        LetStarBacktracker(Env env,
                           ScamExpr * sym,
                           BacktrackHandle backtracker);

        void run() override;

    private:
        Env             env;
        ScamExpr *      sym;
    };

    class LetStarCont : public LetCommonCont
    {
    public:
        LetStarCont(ScamExpr * formals,
                    ScamExpr * rest,
                    ScamExpr * forms,
                    ContHandle cont,
                    Env env,
                    ScamEngine * engine);

    protected:
        void do_let(ScamExpr * expr) override;

    private:
        ScamExpr * formals;
        ScamExpr * rest;
        Env        env;
        ScamEngine * engine;

        void makeBacktracker(ScamExpr * sym) const;
    };

    class LetStepCont : public Continuation
    {
    public:
        LetStepCont(ScamExpr * formals,
                    ScamExpr * forms,
                    ScamExpr * evaled,
                    ScamExpr * args,
                    ContHandle cont,
                    Env env,
                    bool rebind);

        void run(ScamExpr * expr) override;

    private:
        ScamExpr * formals;
        ScamExpr * forms;
        ScamExpr * evaled;
        ScamExpr * args;
        ContHandle cont;
        Env env;
        bool rebind;
    };

    class LetBaseWorker : public Worker
    {
    public:
        LetBaseWorker(char const * name,
                      ScamExpr * args,
                      ContHandle cont,
                      Env env);

        void run() override;

    protected:
        ContHandle cont;
        Env env;

        virtual void
        do_next(ScamExpr * formals, ScamExpr * values, ScamExpr * forms) = 0;

    private:
        ScamExpr * args;

        void report_error();

        /**
         * verify_single
         *
         * Verify that the given expression is a list of the form
         * "(sym expr)"
         */
        bool verify_single(ScamExpr * arg);

        /**
         * verify_next
         *
         * Verify that the current argument is valid.
         *
         * If so, continue checking the rest of list.
         *
         */
        bool verify_list(ScamExpr * check);

        /**
         * Verify the argument list is structurally sound.
         *
         * \@args is well-formed if it a list of zero or more pairs
         * where the first item is a symbol and the second is any
         * form.
         *
         * @return true iff the args is structurally correct.
         */
        bool verify_args();

        ScamExpr * parse_bindings(ScamExpr * bindings);
        ScamExpr * parse_args();
    };

    class LetWorker : public LetBaseWorker
    {
    public:
        LetWorker(ScamExpr * args, ContHandle cont, Env env, bool rebind);

    protected:
        void do_next(ScamExpr * formals,
                     ScamExpr * values,
                     ScamExpr * forms) override;

    private:
        const bool rebind;
    };

    class LetStarWorker : public LetBaseWorker
    {
    public:
        LetStarWorker(ScamExpr * args,
                      ContHandle cont,
                      Env env,
                      ScamEngine * engine);

    protected:
        void do_next(ScamExpr * formals,
                     ScamExpr * values,
                     ScamExpr * forms) override;

    private:
        ScamEngine * engine;
    };

    class LetEvalWorker : public Worker
    {
    public:
        LetEvalWorker(ScamExpr * formals,
                      ScamExpr * evaled,
                      ScamExpr * args,
                      ScamExpr * forms,
                      ContHandle cont,
                      Env env,
                      bool rebind);

        void run() override;

    private:
        ScamExpr * formals;
        ScamExpr * evaled;
        ScamExpr * args;
        ScamExpr * forms;
        ContHandle cont;
        Env env;
        bool rebind;
    };

    void let_impl(ScamExpr * args, ContHandle cont, Env env, bool rebind)
    {
        workQueueHelper<LetWorker>(args, cont, env, rebind);
    }

    void letstar_impl(ScamExpr * args,
                      ContHandle cont,
                      Env env,
                      ScamEngine * engine)
    {
        workQueueHelper<LetStarWorker>(args, cont, env, engine);
    }

    //** class implementations **//

    LetCommonCont::LetCommonCont(char const * name,
                                 ScamExpr * forms,
                                 ContHandle cont)
        : Continuation(name)
        , forms(forms)
        , cont(cont)
    {
    }

    void LetCommonCont::run(ScamExpr * expr)
    {
        Continuation::run(expr);

        if ( expr->error() ) {
            cont->run(expr);
        }
        else {
            do_let(expr);
        }
    }

    void LetCommonCont::final_eval(Env env)
    {
        using WT = EvalWorker;
        ScamExpr * f = forms;
        workQueueHelper<WT>(f, env, cont);
    }

    ////////////

    LetCont::LetCont(ScamExpr * formals,
                     ScamExpr * forms,
                     ContHandle cont,
                     Env env,
                     bool rebind)
        : LetCommonCont("Let", forms, cont)
        , formals(formals)
        , env(env)
        , rebind(rebind)
    {
    }

    void LetCont::do_let(ScamExpr * expr)
    {
        Binder binder(env);
        ScamExpr * ff = formals;
        Env extended = binder.bind(ff, expr);

        rebind_procs(extended);
        final_eval(extended);
    }

    void LetCont::rebind_procs(Env extended)
    {
        if ( ! rebind ) {
            return;
        }

        const size_t len = formals->length();
        for ( size_t n = 0 ; n < len ; ++n ) {
            ScamExpr * k = formals->nthcar(n);
            ScamExpr * v = extended.get(k);
            if ( v->isProcedure() ) {
                ScamExpr * newV = v->withEnvUpdate(extended);
                extended.assign(k, newV);
            }
        }
    }

    /////////////////////////

    LetStarBacktracker::LetStarBacktracker(Env env,
                                           ScamExpr * sym,
                                           BacktrackHandle backtracker)
        : Backtracker("Let*", backtracker)
        , env(env)
        , sym(sym)
    {
    }

    void LetStarBacktracker::run()
    {
        Backtracker::run();
        env.remove(sym);
        shared_ptr<Continuation> cont
            = make_shared<Continuation>("Assign Backtrack");
        runParent(cont);
    }

    /////////////////////////

    LetStarCont::LetStarCont(ScamExpr * formals,
                             ScamExpr * rest,
                             ScamExpr * forms,
                             ContHandle cont,
                             Env env,
                             ScamEngine * engine)
        : LetCommonCont("Let*", forms, cont)
        , formals(formals)
        , rest(rest)
        , env(env)
        , engine(engine)
    {
    }

    void LetStarCont::do_let(ScamExpr * expr)
    {
        if ( formals->isNil() ) {
            final_eval(env);
        }
        else {
            ScamExpr * sym = formals->getCar();
            env.put(sym, expr);

            makeBacktracker(sym);

            ScamExpr * safe = safeCons(rest);
            using C = LetStarCont;
            ContHandle ch = make_shared<C>(formals->getCdr(),
                                           safe->getCdr(),
                                           forms,
                                           cont,
                                           env,
                                           engine);
            safe->getCar()->eval(ch, env);
        }
    }

    void LetStarCont::makeBacktracker(ScamExpr * sym) const
    {
        BacktrackHandle backtracker = engine->getBacktracker();
        BacktrackHandle newBT =
            make_shared<LetStarBacktracker>(env, sym, backtracker);
        engine->setBacktracker(newBT);
    }

    ///////////

    LetBaseWorker::LetBaseWorker(char const * name,
                                 ScamExpr * args,
                                 ContHandle cont,
                                 Env env)
        : Worker(name)
        , cont(cont)
        , env(env)
        , args(args)
    {
    }

    void LetBaseWorker::run()
    {
        Worker::run();

        if ( ! verify_args() ) {
            return;
        }

        ScamExpr * parsed  = parse_args();
        ScamExpr * formals = parsed->getCar()->getCar();
        ScamExpr * values  = parsed->getCar()->getCdr();
        ScamExpr * forms   = parsed->getCdr();

        do_next(formals, values, forms);
    }

    void LetBaseWorker::report_error()
    {
        stringstream s;
        s << "Expected (((sym form)...) forms...); got "
          << args->toString();
        ScamExpr * err = ExpressionFactory::makeError(s.str());

        cont->run(err);
    }

    bool LetBaseWorker::verify_single(ScamExpr * arg)
    {
        if ( ! arg->isList() || 2 != arg->length() ) {
            report_error();
            return false;
        }

        if ( ! arg->getCar()->isSymbol() ) {
            report_error();
            return false;
        }

        return true;
    }

    bool LetBaseWorker::verify_list(ScamExpr * check)
    {
        if ( check->isNil() ) {
            return true;
        }

        if ( ! check->isList() ) {
            report_error();
            return false;
        }

        if ( ! verify_single(check->getCar()) ) {
            return false;
        }

        return verify_list(check->getCdr());
    }

    bool LetBaseWorker::verify_args()
    {
        if ( ! args->isList() || args->isNil() ) {
            report_error();
            return false;
        }

        ScamExpr * check = args->getCar();
        return verify_list(check);
    }

    ScamExpr * LetBaseWorker::parse_bindings(ScamExpr * bindings)
    {
        if ( bindings->isNil() ) {
            ScamExpr * nil = ExpressionFactory::makeNil();
            return ExpressionFactory::makeCons(nil, nil);
        }

        ScamExpr * one  = bindings->getCar();
        ScamExpr * rest = bindings->getCdr();

        ScamExpr * separated = parse_bindings(rest);

        ScamExpr * symList
            = ExpressionFactory::makeCons(one->getCar(),
                                          separated->getCar());
        ScamExpr * valList
            = ExpressionFactory::makeCons(one->getCdr()->getCar(),
                                          separated->getCdr());

        return ExpressionFactory::makeCons(symList, valList);
    }

    ScamExpr * LetBaseWorker::parse_args()
    {
        ScamExpr * forms    = args->getCdr();
        ScamExpr * bindings = args->getCar();

        ScamExpr * separated = parse_bindings(bindings);

        return ExpressionFactory::makeCons(separated, forms);
    }

    /////////////

    LetWorker::LetWorker(ScamExpr * args, ContHandle cont, Env env, bool rebind)
        : LetBaseWorker("Let", args, cont, env)
        , rebind(rebind)
    {
    }

    void LetWorker::do_next(ScamExpr * formals,
                            ScamExpr * values,
                            ScamExpr * forms)
    {
        ScamExpr * evaled = ExpressionFactory::makeNil();
        ScamExpr * e = evaled;
        workQueueHelper<LetEvalWorker>(formals,
                                       e,
                                       values,
                                       forms,
                                       cont,
                                       env,
                                       rebind);
    }

    ////////////

    LetStarWorker::LetStarWorker(ScamExpr * args,
                                 ContHandle cont,
                                 Env env,
                                 ScamEngine * engine)
        : LetBaseWorker("LetStar", args, cont, env)
        , engine(engine)
    {
    }

    void LetStarWorker::do_next(ScamExpr * formals,
                                ScamExpr * values,
                                ScamExpr * forms)
    {
        Env extended = env.extend();
        ScamExpr * safe = safeCons(values);

        using C = LetStarCont;
        ContHandle ch = make_shared<C>(formals,
                                       safe->getCdr(),
                                       forms,
                                       cont,
                                       extended,
                                       engine);
        safe->getCar()->eval(ch, env);
    }

    LetEvalWorker::LetEvalWorker(ScamExpr * formals,
                                 ScamExpr * evaled,
                                 ScamExpr * args,
                                 ScamExpr * forms,
                                 ContHandle cont,
                                 Env env,
                                 bool rebind)
        : Worker("LetEvalWorker")
        , formals(formals)
        , evaled(evaled)
        , args(args)
        , forms(forms)
        , cont(cont)
        , env(env)
        , rebind(rebind)
    {
    }

    void LetEvalWorker::run()
    {
        Worker::run();

        if ( args->length() > 0 ) {
            ScamExpr * car = args->nthcar(0);
            ScamExpr * cdr = args->nthcdr(0);

            ContHandle ch
                = make_shared<LetStepCont>(formals,
                                           forms,
                                           evaled,
                                           cdr,
                                           cont,
                                           env,
                                           rebind);
            car->eval(ch, env);
        }
        else {
            ContHandle ch
                = make_shared<LetCont>(formals,
                                       forms,
                                       cont,
                                       env,
                                       rebind);
            ch->run(evaled);
        }
    }

    LetStepCont::LetStepCont(ScamExpr * formals,
                             ScamExpr * forms,
                             ScamExpr * evaled,
                             ScamExpr * args,
                             ContHandle cont,
                             Env env,
                             bool rebind)
        : Continuation("LetStepCont")
        , formals(formals)
        , forms(forms)
        , evaled(evaled)
        , args(args)
        , cont(cont)
        , env(env)
        , rebind(rebind)
    {
    }

    void LetStepCont::run(ScamExpr * expr)
    {
        if ( expr->error() ) {
            cont->run(expr);
        }
        else {
            ScamListAdapter a(evaled);
            ScamExpr * extend = a.append(expr);
            ScamExpr * p[] = { formals, extend, args, forms };
            using WT = LetEvalWorker;
            workQueueHelper<WT>(p[0], p[1], p[2], p[3], cont, env, rebind);
        }
    }
}
