
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
    extern void apply_impl(ExprHandle const & args, ContHandle cont, Env & env);
}

Or::Or()
    : SpecialForm("or")
{
}

void Or::apply(ExprHandle const & args, ContHandle cont, Env & env)
{
    apply_impl(args, cont, env);
}

ExprHandle Or::clone() const
{
    return ExpressionFactory::makeForm<Or>();
}

namespace
{
    class OrWorker : public Worker
    {
    public:
        OrWorker(ExprHandle const & args, ContHandle cont, Env env, size_t n);
        void run() override;

    private:
        ExprHandle const & args;
        ContHandle cont;
        Env env;
        size_t n;
    };

    class OrCont : public Continuation
    {
    public:
        OrCont(ExprHandle const & args, ContHandle cont, Env & env, size_t n);
        void run(ExprHandle expr) const override;

    private:
        ExprHandle const & args;
        ContHandle cont;
        Env & env;
        size_t n;
    };

    void apply_impl(ExprHandle const & args, ContHandle cont, Env & env)
    {
        shared_ptr<OrWorker> thunk
            = make_shared<OrWorker>(args, cont, env, 0u);
        WorkerHandle start = thunk;
        GlobalWorkQueue.put(start);
    }
}

OrWorker::OrWorker(ExprHandle const & args,
                     ContHandle cont,
                     Env env,
                     size_t n)
    : args(args)
    , cont(cont)
    , env(env)
    , n(n)
{
}

void OrWorker::run()
{
    if ( ! args->isList() ) {
        stringstream s;
        s << "Or expects a list of forms; got: " << args->toString();
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return;
    }

    size_t const len = args->length();
    if ( 0 == len ) {
        cont->run(ExpressionFactory::makeBoolean(false));
    }
    else if ( n == (len - 1) ) {
        args->nth(len-1)->eval(cont, env);
    }
    else {
        ExprHandle test = args->nth(n);

        ContHandle newCont = make_shared<OrCont>(args, cont, env, n+1);
        test->eval(newCont, env);
    }
}

OrCont::OrCont(ExprHandle const & args, ContHandle cont, Env & env, size_t n)
    : args(args)
    , cont(cont)
    , env(env)
    , n(n)
{
}

void OrCont::run(ExprHandle expr) const
{
    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( expr->truth() ) {
        cont->run(expr);
    }
    else {
        shared_ptr<OrWorker> thunk
            = make_shared<OrWorker>(args, cont, env, n);
        WorkerHandle start = thunk;
        GlobalWorkQueue.put(start);
    }
}
