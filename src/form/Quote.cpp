
#include "form/Quote.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Quote::Quote()
    : SpecialForm("quote")
{
}

Quote * Quote::makeInstance()
{
    static Quote quote;
    return &quote;
}

void Quote::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    ScamExpr * expr = args->getCar();
    cont->run(expr);
}

QuasiQuote::QuasiQuote()
    : SpecialForm("quasiquote")
{
}

QuasiQuote * QuasiQuote::makeInstance()
{
    static QuasiQuote instance;
    return &instance;
}

namespace
{
    extern void qq_apply(ScamExpr * args,
                         Continuation * cont,
                         Env * env,
                         bool top);
}

void QuasiQuote::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    qq_apply(args, cont, env, true);
}

namespace
{
    static ScamExpr * const spliceTag =
        ExpressionFactory::makeSymbol("**splicing**", false);

    class  QQConsListCdrCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        QQConsListCdrCont(ScamExpr * car, Continuation * cont, Env * env)
            : Continuation("QQConsListCdrCont")
            , car(car)
            , cont(cont)
            , env(env)
        {
        }

        static QQConsListCdrCont *
        makeInstance(ScamExpr * car, Continuation * cont, Env * env)
        {
            return new  QQConsListCdrCont(car, cont, env);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                car->mark();
                cont->mark();
                env->mark();
            }
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
        ScamExpr * car;
        Continuation * cont;
        Env  *      env;

        void handle(ScamExpr * expr)
        {
            if ( ! check_splice(expr) ) {
                ScamExpr * rv = ExpressionFactory::makeCons(car, expr);
                cont->run(rv);
            }
        }

        bool check_splice(ScamExpr * expr)
        {
            if ( car->isCons() ) {
                ScamExpr * first = car->nthcar(0);
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
            ScamExpr * f = expr;
            size_t count = car->length();

            while ( --count > 0 ) {
                ScamExpr * form = car->nthcar(count);
                f = ExpressionFactory::makeCons(form, f);
            }

            cont->run(f);
        }
    };

    class  QQConsListCarCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        QQConsListCarCont(ScamExpr * cdr, Continuation * cont, Env * env)
            : Continuation("QQConsListCarCont")
            , cdr(cdr)
            , cont(cont)
            , env(env)
        {
        }

        static QQConsListCarCont *
        makeInstance(ScamExpr * cdr, Continuation * cont, Env * env)
        {
            return new QQConsListCarCont(cdr, cont, env);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                cdr->mark();
                cont->mark();
                env->mark();
            }
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);
            if ( expr->error() ) {
                cont->run(expr);
            }
            else {
                Continuation * h =
                    standardMemoryManager.make<QQConsListCdrCont>(expr,
                                                                  cont,
                                                                  env);
                qq_apply(cdr, h, env, false);
            }
        }

    private:
        ScamExpr * cdr;
        Continuation * cont;
        Env *       env;
    };

    class  QQSpliceCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        QQSpliceCont(Continuation * cont)
            : Continuation("QQSpliceCont")
            , cont(cont)
        {
        }

        static QQSpliceCont * makeInstance(Continuation * cont)
        {
            return new QQSpliceCont(cont);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
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
                ScamExpr * internal =
                    ExpressionFactory::makeCons(spliceTag, expr);
                cont->run(internal);
            }
        }

    private:
        Continuation * cont;
    };

    class QuasiQuoteWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        QuasiQuoteWorker(ScamExpr * form, Continuation * cont, Env * env)
            : Worker("QuasiQuote")
            , form(form)
            , cont(cont)
            , env(env)
        {
        }

        static QuasiQuoteWorker *
        makeInstance(ScamExpr * form, Continuation * cont, Env * env)
        {
            return new QuasiQuoteWorker(form, cont, env);
        }

    public:
        void mark() const override
       {
           if ( ! isMarked() ) {
             Worker::mark();
             form->mark();
             cont->mark();
             env->mark();
         }
       }


        void run() override
        {
            build_qq_form(form, cont);
        }

    private:
        ScamExpr * form;
        Continuation * cont;
        Env *       env;

        bool verify_single_form(ScamExpr * input, Continuation * cont)
        {
            if ( ! input->isList() || 1 != input->length() ) {
                stringstream s;
                s << "expected single form, got " << input->toString();
                ScamExpr * err = ExpressionFactory::makeError(s.str());
                cont->run(err);
                return false;
            }
            return true;
        }

        void unquote_form(ScamExpr * input, Continuation * cont)
        {
            if ( verify_single_form(input, cont) ) {
                ScamExpr * form = input->nthcar(0);
                form->eval(cont, env);
            }
        }

        void splice_form(ScamExpr * input, Continuation * cont)
        {
            if ( verify_single_form(input, cont) ) {
                Continuation * h =
                    standardMemoryManager.make<QQSpliceCont>(cont);

                ScamExpr * form = input->nthcar(0);
                form->eval(h, env);
            }
        }

        void cons_qq_list(ScamExpr * car, ScamExpr * cdr, Continuation * cont)
        {
            Continuation * h =
                standardMemoryManager.make<QQConsListCarCont>(cdr, cont, env);
            build_qq_form(car, h);
        }

        void build_qq_list(ScamExpr * input, Continuation * cont)
        {
            ScamExpr * first = input->nthcar(0);
            ScamExpr * rest  = input->nthcdr(0);

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

        void build_qq_form(ScamExpr * input, Continuation * cont)
        {
            if ( ! input->isList() || input->isNil() ) {
                cont->run(input);
            }
            else {
                build_qq_list(input, cont);
            }
        }
    };

    void qq_apply(ScamExpr * args, Continuation * cont, Env * env, bool top)
    {
        if ( ! args->isList() ) {
            stringstream s;
            s << "quasiquote expecting list of args, got " << args->toString();
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
        }
        else if ( top ) {
            if ( 1 != args->length() ) {
                stringstream s;
                s << "quasiquote expecting one form, got " << args->toString();
                ScamExpr * err = ExpressionFactory::makeError(s.str());
                cont->run(err);
            }
            else {
                ScamExpr * form = args->nthcar(0);
                workQueueHelper<QuasiQuoteWorker>(form, cont, env);
            }
        }
        else {
            ScamExpr * form = args;
            workQueueHelper<QuasiQuoteWorker>(form, cont, env);
        }
    }
}
