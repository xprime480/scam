
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
    extern void apply_impl(ScamExpr * args, ContHandle cont, Env env);
}

Let::Let()
    : SpecialForm("let")
{
}

void Let::apply(ScamExpr * args, ContHandle cont, Env env)
{
    apply_impl(args, cont, env);
}

namespace
{
    class LetCont : public Continuation
    {
    public:
        LetCont(ScamExpr * formals, ScamExpr * forms, ContHandle cont, Env env)
            : Continuation("Let")
            , formals(formals->clone())
            , forms(forms->clone())
            , cont(cont)
            , env(env)
        {
        }

        void run(ScamExpr * expr)
        {
            if ( expr->error() ) {
                cont->run(expr);
            }
            else {
                do_let(expr);
            }
        }

    private:
        ExprHandle formals;
        ExprHandle forms;
        ContHandle cont;
        Env        env;

        void do_let(ScamExpr * expr)
        {
            Binder binder(env);
            ScamExpr * ff = formals.get();
            Env extended = binder.bind(ff, expr);


            using WT = EvalWorker;
            ScamExpr * f = forms.get();
            workQueueHelper<WT>(f, extended, cont);
        }

    };

    class LetWorker : public Worker
    {
    public:
        LetWorker(ScamExpr * args, ContHandle cont, Env env)
            : Worker("Let")
            , args(args->clone())
            , cont(cont)
            , env(env)
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

            ContHandle ch = make_shared<LetCont>(formals, forms, cont, env);
            values->mapEval(ch, env);
        }

    private:
        ExprHandle args;
        ContHandle cont;
        Env env;

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

    void apply_impl(ScamExpr * args, ContHandle cont, Env env)
    {
        workQueueHelper<LetWorker>(args, cont, env);
    }
}
