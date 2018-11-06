
#include "form/Let.hpp"

#include "Binder.hpp"
#include "Continuation.hpp"
#include "EvalWorker.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

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
                             Env env);
}

Let::Let()
    : SpecialForm("let")
{
}

void Let::apply(ScamExpr * args, ContHandle cont, Env env)
{
    let_impl(args, cont, env, false);
}

LetStar::LetStar()
    : SpecialForm("let*")
{
}

void LetStar::apply(ScamExpr * args, ContHandle cont, Env env)
{
    letstar_impl(args, cont, env);
}


LetRec::LetRec()
    : SpecialForm("letrec")
{
}

void LetRec::apply(ScamExpr * args, ContHandle cont, Env env)
{
    let_impl(args, cont, env, true);
}

namespace
{
    ExprHandle safeCons(ScamExpr * expr)
    {
        if ( expr->isCons() ) {
            return expr->clone();
        }
        return ExpressionFactory::makeList(expr);
    }

    class LetCommonCont : public Continuation
    {
    public:
        LetCommonCont(char const * name, ScamExpr * forms, ContHandle cont)
            : Continuation(name)
            , forms(forms->clone())
            , cont(cont)
        {
        }

        void run(ScamExpr * expr) override
        {
            if ( expr->error() ) {
                cont->run(expr);
            }
            else {
                do_let(expr);
            }
        }

    protected:
        ExprHandle forms;
        ContHandle cont;

        virtual void do_let(ScamExpr * expr) = 0;

        void final_eval(Env env)
        {
            using WT = EvalWorker;
            ScamExpr * f = forms.get();
            workQueueHelper<WT>(f, env, cont);
        }
    };

    class LetCont : public LetCommonCont
    {
    public:
        LetCont(ScamExpr * formals,
                ScamExpr * forms,
                ContHandle cont,
                Env env,
                bool rebind)
            : LetCommonCont("Let", forms, cont)
            , formals(formals->clone())
            , env(env)
            , rebind(rebind)
        {
        }

    protected:
        void do_let(ScamExpr * expr) override
        {
            Binder binder(env);
            ScamExpr * ff = formals.get();
            Env extended = binder.bind(ff, expr);

            rebind_procs(extended);
            final_eval(extended);
        }

    private:
        ExprHandle formals;
        Env        env;
        bool       rebind;

        void rebind_procs(Env extended)
        {
            if ( ! rebind ) {
                return;
            }

            const size_t len = formals->length();
            for ( size_t n = 0 ; n < len ; ++n ) {
                ScamExpr * k = formals->nthcar(n).get();
                ScamExpr * v = extended.get(k).get();
                if ( v->isProcedure() ) {
                    ExprHandle newV = v->withEnvUpdate(extended);
                    extended.assign(k, newV.get());
                }
            }
        }
    };

    class LetStarCont : public LetCommonCont
    {
    public:
        LetStarCont(ScamExpr * formals,
                    ScamExpr * rest,
                    ScamExpr * forms,
                    ContHandle cont,
                    Env env)
            : LetCommonCont("Let*", forms, cont)
            , formals(formals->clone())
            , rest(rest->clone())
            , env(env)
        {
        }

    protected:
        void do_let(ScamExpr * expr) override
        {
            if ( formals->isNil() ) {
                final_eval(env);
            }
            else {
                env.put(formals->getCar().get(), expr);
                ExprHandle safe = safeCons(rest.get());

                using C = LetStarCont;
                ContHandle ch = make_shared<C>(formals->getCdr().get(),
                                               safe->getCdr().get(),
                                               forms.get(),
                                               cont,
                                               env);
                safe->getCar()->eval(ch, env);
            }
        }

    private:
        ExprHandle formals;
        ExprHandle rest;
        Env        env;
    };

    class LetBaseWorker : public Worker
    {
    public:
        LetBaseWorker(char const * name,
                      ScamExpr * args,
                      ContHandle cont,
                      Env env)
            : Worker(name)
            , cont(cont)
            , env(env)
            , args(args->clone())
        {
        }

        void run()
        {
            if ( ! verify_args() ) {
                return;
            }

            ExprHandle parsed  = parse_args();
            ScamExpr * formals = parsed->getCar()->getCar().get();
            ScamExpr * values  = parsed->getCar()->getCdr().get();
            ScamExpr * forms   = parsed->getCdr().get();

            do_next(formals, values, forms);
        }

    protected:
        ContHandle cont;
        Env env;

        virtual void
        do_next(ScamExpr * formals, ScamExpr * values, ScamExpr * forms) = 0;

    private:
        ExprHandle args;

        void report_error()
        {
            stringstream s;
            s << "Expected (((sym form)...) forms...); got "
              << args->toString();
            ExprHandle err = ExpressionFactory::makeError(s.str());

            cont->run(err.get());
        }

        /**
         * verify_single
         *
         * Verify that the given expression is a list of the form
         * "(sym expr)"
         */
        bool verify_single(ScamExpr * arg)
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

        /**
         * verify_next
         *
         * Verify that the current argument is valid.
         *
         * If so, continue checking the rest of list.
         *
         */
        bool verify_list(ScamExpr * check)
        {
            if ( check->isNil() ) {
                return true;
            }

            if ( ! check->isList() ) {
                report_error();
                return false;
            }

            if ( ! verify_single(check->getCar().get()) ) {
                return false;
            }

            return verify_list(check->getCdr().get());
        }

        /**
         * Verify the argument list is structurally sound.
         *
         * \@args is well-formed if it a list of zero or more pairs
         * where the first item is a symbol and the second is any
         * form.
         *
         * @return true iff the args is structurally correct.
         */
        bool verify_args()
        {
            if ( ! args->isList() || args->isNil() ) {
                report_error();
                return false;
            }

            ScamExpr * check = args->getCar().get();
            return verify_list(check);
        }

        ExprHandle parse_bindings(ScamExpr * bindings)
        {
            if ( bindings->isNil() ) {
                ScamExpr * nil = ExpressionFactory::makeNil().get();
                return ExpressionFactory::makeCons(nil, nil);
            }

            ScamExpr * one  = bindings->getCar().get();
            ScamExpr * rest = bindings->getCdr().get();

            ExprHandle separated = parse_bindings(rest);

            ExprHandle symList
                = ExpressionFactory::makeCons(one->getCar().get(),
                                              separated->getCar().get());
            ExprHandle valList
                = ExpressionFactory::makeCons(one->getCdr()->getCar().get(),
                                              separated->getCdr().get());

            return ExpressionFactory::makeCons(symList.get(), valList.get());
        }

        ExprHandle parse_args()
        {
            ScamExpr * forms    = args->getCdr().get();
            ScamExpr * bindings = args->getCar().get();

            ExprHandle separated = parse_bindings(bindings);

            return ExpressionFactory::makeCons(separated.get(), forms);
        }
    };

    class LetWorker : public LetBaseWorker
    {
    public:
        LetWorker(ScamExpr * args, ContHandle cont, Env env, bool rebind)
            : LetBaseWorker("Let", args, cont, env)
            , rebind(rebind)
        {
        }

    protected:
        void do_next(ScamExpr * formals,
                     ScamExpr * values,
                     ScamExpr * forms) override
        {
            ContHandle ch
                = make_shared<LetCont>(formals, forms, cont, env, rebind);
            values->mapEval(ch, env);
        }

    private:
        const bool rebind;
    };

    class LetStarWorker : public LetBaseWorker
    {
    public:
        LetStarWorker(ScamExpr * args, ContHandle cont, Env env)
            : LetBaseWorker("LetStar", args, cont, env)
        {
        }

    protected:
        void do_next(ScamExpr * formals,
                     ScamExpr * values,
                     ScamExpr * forms) override
        {
            Env extended = env.extend();
            ExprHandle safe = safeCons(values);

            using C = LetStarCont;
            ContHandle ch = make_shared<C>(formals,
                                           safe->getCdr().get(),
                                           forms,
                                           cont,
                                           extended);
            safe->getCar()->eval(ch, env);
        }

    private:
    };

    void let_impl(ScamExpr * args, ContHandle cont, Env env, bool rebind)
    {
        workQueueHelper<LetWorker>(args, cont, env, rebind);
    }

    void letstar_impl(ScamExpr * args, ContHandle cont, Env env)
    {
        workQueueHelper<LetStarWorker>(args, cont, env);
    }
}
