
#include "form/And.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void apply_impl(ScamExpr * args, ContHandle cont, Env env);
}

And::And()
    : SpecialForm("and")
{
}

void And::apply(ScamExpr * args, ContHandle cont, Env env)
{
    apply_impl(args, cont, env);
}

namespace
{
    class AndWorker : public Worker
    {
    public:
        AndWorker(ContHandle cont, Env env, ScamExpr * args, size_t n);
        void run() override;

    private:
        ExprHandle args;
        ContHandle cont;
        Env env;
        size_t n;
    };

    class AndCont : public Continuation
    {
    public:
        AndCont(ScamExpr * args, ContHandle cont, Env env, size_t n);
        void run(ScamExpr * expr) override;

    private:
        ExprHandle args;
        ContHandle cont;
        Env env;
        size_t n;
    };

    void apply_impl(ScamExpr * args, ContHandle cont, Env env)
    {
        unsigned pos { 0 };
        workQueueHelper<AndWorker>(cont, env, args, pos);
    }
}

AndWorker::AndWorker(ContHandle cont,
                     Env env,
                     ScamExpr * args,
                     size_t n)
    : Worker("And")
    , args(args->clone())
    , cont(cont)
    , env(env)
    , n(n)
{
}

void AndWorker::run()
{
    Worker::run();

    if ( ! args->isList() ) {
        stringstream s;
        s << "And expects a list of forms; got: " << args->toString();
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err.get());
        return;
    }

    size_t const len = args->length();
    if ( 0 == len ) {
        cont->run(ExpressionFactory::makeBoolean(true).get());
    }
    else if ( n == (len - 1) ) {
        args->nth(len-1)->eval(cont, env);
    }
    else {
        ExprHandle test = args->nth(n);
	ScamExpr * a = args.get();
        ContHandle newCont = make_shared<AndCont>(a, cont, env, n+1);
        test->eval(newCont, env);
    }
}

AndCont::AndCont(ScamExpr * args, ContHandle cont, Env env, size_t n)
    : Continuation("And")
    , args(args->clone())
    , cont(cont)
    , env(env)
    , n(n)
{
}

void AndCont::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( ! expr->truth() ) {
        cont->run(expr);
    }
    else {
	ScamExpr * a = args.get();
        workQueueHelper<AndWorker>(cont, env, a, n);
    }
}
