
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
    extern void apply_impl(ScamExpr * args, Continuation * cont, Env env);
}

And::And()
    : SpecialForm("and")
{
}

And * And::makeInstance()
{
    static And instance;
    return &instance;
}

void And::apply(ScamExpr * args, Continuation * cont, Env env)
{
    apply_impl(args, cont, env);
}

namespace
{
    class AndWorker : public Worker
    {
    public:
        AndWorker(Continuation * cont, Env env, ScamExpr * args, size_t n);
        void run() override;

    private:
        ScamExpr * args;
        Continuation * cont;
        Env env;
        size_t n;
    };

    class AndCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        AndCont(ScamExpr * args, Continuation * cont, Env env, size_t n);

        static AndCont *
        makeInstance(ScamExpr * args, Continuation * cont, Env env, size_t n);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    private:
        ScamExpr * args;
        Continuation * cont;
        Env env;
        size_t n;
    };

    void apply_impl(ScamExpr * args, Continuation * cont, Env env)
    {
        unsigned pos { 0 };
        workQueueHelper<AndWorker>(cont, env, args, pos);
    }
}

AndWorker::AndWorker(Continuation * cont,
                     Env env,
                     ScamExpr * args,
                     size_t n)
    : Worker("And")
    , args(args)
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
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return;
    }

    size_t const len = args->length();
    if ( 0 == len ) {
        cont->run(ExpressionFactory::makeBoolean(true));
    }
    else if ( n == (len - 1) ) {
        args->nthcar(len-1)->eval(cont, env);
    }
    else {
        ScamExpr * test = args->nthcar(n);
        ScamExpr * a = args;
        Continuation * newCont =
            standardMemoryManager.make<AndCont>(a, cont, env, n+1);
        test->eval(newCont, env);
    }
}

AndCont::AndCont(ScamExpr * args, Continuation * cont, Env env, size_t n)
    : Continuation("And")
    , args(args)
    , cont(cont)
    , env(env)
    , n(n)
{
}

AndCont *
AndCont::makeInstance(ScamExpr * args, Continuation * cont, Env env, size_t n)
{
    return new AndCont(args, cont, env, n);
}

void AndCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
    }
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
        workQueueHelper<AndWorker>(cont, env, args, n);
    }
}
