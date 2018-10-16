
#include "form/Or.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
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

Or::Or()
    : SpecialForm("or")
{
}

void Or::apply(ScamExpr * args, ContHandle cont, Env env)
{
    apply_impl(args, cont, env);
}

namespace
{
    class OrWorker : public Worker
    {
    public:
        OrWorker(ContHandle cont, Env env, ScamExpr * args, size_t n);
        void run() override;

    private:
        ExprHandle args;
        ContHandle cont;
        Env env;
        size_t n;
    };

    class OrCont : public Continuation
    {
    public:
        OrCont(ScamExpr * args, ContHandle cont, Env env, size_t n);
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
        workQueueHelper<OrWorker>(cont, env, args, pos);
    }
}

OrWorker::OrWorker(ContHandle cont,
                   Env env,
                   ScamExpr * args,
                   size_t n)
    : Worker("Or")
    , args(args->clone())
    , cont(cont)
    , env(env)
    , n(n)
{
}

void OrWorker::run()
{
    Worker::run();

    if ( ! args->isList() ) {
        stringstream s;
        s << "Or expects a list of forms; got: " << args->toString();
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err.get());
        return;
    }

    size_t const len = args->length();
    if ( 0 == len ) {
        cont->run(ExpressionFactory::makeBoolean(false).get());
    }
    else if ( n == (len - 1) ) {
        args->nth(len-1)->eval(cont, env);
    }
    else {
        ExprHandle test = args->nth(n);

        ContHandle newCont = make_shared<OrCont>(args.get(), cont, env, n+1);
        test->eval(newCont, env);
    }
}

OrCont::OrCont(ScamExpr * args, ContHandle cont, Env env, size_t n)
    : Continuation("Or")
    , args(args->clone())
    , cont(cont)
    , env(env)
    , n(n)
{
}

void OrCont::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( expr->truth() ) {
        cont->run(expr);
    }
    else {
        ScamExpr * e = args.get();
        workQueueHelper<OrWorker>(cont, env, e, n);
    }
}
