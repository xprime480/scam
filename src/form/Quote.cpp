
#include "form/Quote.hpp"

#include "Continuation.hpp"
#include "Extractor.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

Quote::Quote()
    : SpecialForm("quote")
{
}

void Quote::apply(ScamExpr * args, ContHandle cont, Env env)
{
    ExprHandle expr = args->getCar();
    cont->run(expr.get());
}

QuasiQuote::QuasiQuote()
    : SpecialForm("quasiquote")
{
}

namespace
{
    extern void qq_apply(ScamExpr * args, ContHandle cont, Env env, bool top);
}

void QuasiQuote::apply(ScamExpr * args, ContHandle cont, Env env)
{
    qq_apply(args, cont, env, true);
}

namespace
{
    static const ExprHandle spliceTag =
        ExpressionFactory::makeSymbol("**splicing**");

    class  QQConsListCdrCont : public Continuation
    {
    public:
        QQConsListCdrCont(ScamExpr * car, ContHandle cont, Env env)
            : Continuation("QQConsListCdrCont")
            , car(car->clone())
            , cont(cont)
            , env(env)
        {
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);
            if ( expr->error() ) {
                cont->run(expr);
            }
            else {
                handle(expr);
            }
        }

    private:
        ExprHandle car;
        ContHandle cont;
        Env        env;

        void handle(ScamExpr * expr)
        {
            if ( ! check_splice(expr) ) {
                ExprHandle final = ExpressionFactory::makeCons(car.get(), expr);
                cont->run(final.get());
            }
        }

        bool check_splice(ScamExpr * expr)
        {
            if ( car->isCons() ) {
                ExprHandle first = car->nthcar(0);
                if ( first->isSymbol() ) {
                    if ( first->toString() == spliceTag->toString() ) {
                        do_splice(expr);
                        return true;
                    }
                }
            }

            return false;
        }

        void do_splice(ScamExpr * expr)
        {
            ExprHandle f = expr->clone();
            size_t count = car->length();

            while ( --count > 0 ) {
                ExprHandle form = car->nthcar(count);
                f = ExpressionFactory::makeCons(form.get(), f.get());
            }

            ScamExpr * final = f.get();
            cont->run(final);
        }
    };

    class  QQConsListCarCont : public Continuation
    {
    public:
        QQConsListCarCont(ScamExpr * cdr, ContHandle cont, Env env)
            : Continuation("QQConsListCarCont")
            , cdr(cdr->clone())
            , cont(cont)
            , env(env)
        {
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);
            if ( expr->error() ) {
                cont->run(expr);
            }
            else {
                ContHandle h = make_shared<QQConsListCdrCont>(expr, cont, env);
                qq_apply(cdr.get(), h, env, false);
            }
        }

    private:
        ExprHandle cdr;
        ContHandle cont;
        Env        env;
    };

    class  QQSpliceCont : public Continuation
    {
    public:
        QQSpliceCont(ContHandle cont)
            : Continuation("QQSpliceCont")
            , cont(cont)
        {
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);
            if ( expr->error() ) {
                cont->run(expr);
            }
            else {
                ExprHandle internal = ExpressionFactory::makeCons(spliceTag.get(), expr);
                cont->run(internal.get());
            }
        }

    private:
        ContHandle cont;
    };

    class QuasiQuoteWorker : public Worker
    {
    public:
        QuasiQuoteWorker(ScamExpr * form, ContHandle cont, Env env)
            : Worker("QuasiQuote")
            , form(form->clone())
            , cont(cont)
            , env(env)
        {
        }

        void run() override
        {
            build_qq_form(form.get(), cont);
        }

    private:
        ExprHandle form;
        ContHandle cont;
        Env        env;

        bool verify_single_form(ScamExpr * input, ContHandle cont)
        {
            if ( ! input->isList() || 1 != input->length() ) {
                stringstream s;
                s << "expected single form, got " << input->toString();
                ExprHandle err = ExpressionFactory::makeError(s.str());
                cont->run(err.get());
                return false;
            }
            return true;
        }

        void unquote_form(ScamExpr * input, ContHandle cont)
        {
            if ( verify_single_form(input, cont) ) {
                ScamExpr * form = input->nthcar(0).get();
                form->eval(cont, env);
            }
        }

        void splice_form(ScamExpr * input, ContHandle cont)
        {
            if ( verify_single_form(input, cont) ) {
                ContHandle h = make_shared<QQSpliceCont>(cont);

                ScamExpr * form = input->nthcar(0).get();
                form->eval(h, env);
            }
        }

        void cons_qq_list(ScamExpr * car, ScamExpr * cdr, ContHandle cont)
        {
            ContHandle h = make_shared<QQConsListCarCont>(cdr, cont, env);
            build_qq_form(car, h);
        }

        void build_qq_list(ScamExpr * input, ContHandle cont)
        {
            ScamExpr * first = input->nthcar(0).get();
            ScamExpr * rest  = input->nthcdr(0).get();

            const bool isSym = first->isSymbol();
            string const sym = first->toString();
            if ( isSym && sym == "unquote" ) {
                unquote_form(rest, cont);
            }
            else if ( isSym && sym == "splice" ) {
                splice_form(rest, cont);
            }
            else {
                cons_qq_list(first, rest, cont);
            }
        }

        void build_qq_form(ScamExpr * input, ContHandle cont)
        {
            if ( ! input->isList() || input->isNil() ) {
                cont->run(input);
            }
            else {
                build_qq_list(input, cont);
            }
        }
    };

    void qq_apply(ScamExpr * args, ContHandle cont, Env env, bool top)
    {
        if ( ! args->isList() ) {
            stringstream s;
            s << "quasiquote expecting list of args, got " << args->toString();
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
        }
        else if ( top ) {
            if ( 1 != args->length() ) {
                stringstream s;
                s << "quasiquote expecting one form, got " << args->toString();
                ExprHandle err = ExpressionFactory::makeError(s.str());
                cont->run(err.get());
            }
            else {
                ScamExpr * form = args->nthcar(0).get();
                workQueueHelper<QuasiQuoteWorker>(form, cont, env);
            }
        }
        else {
            ScamExpr * form = args;
            workQueueHelper<QuasiQuoteWorker>(form, cont, env);
        }
    }
}
