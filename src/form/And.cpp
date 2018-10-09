
#include "form/And.hpp"

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

And::And()
    : SpecialForm("and")
{
}

void And::apply(ExprHandle const & args, ContHandle cont, Env & env)
{
    apply_impl(args, cont, env);
}

ExprHandle And::clone() const
{
    return ExpressionFactory::makeForm<And>();
}

namespace
{
    class AndWorker : public Worker
    {
    public:
        AndWorker(ExprHandle const & args, ContHandle cont, Env env, size_t n);
        void run() override;

    private:
        ExprHandle const & args;
        ContHandle cont;
        Env env;
        size_t n;
    };

    class AndCont : public Continuation
    {
    public:
        AndCont(ExprHandle const & args, ContHandle cont, Env & env, size_t n);
        void run(ExprHandle expr) const override;

    private:
        ExprHandle const & args;
        ContHandle cont;
        Env & env;
        size_t n;
    };

    void apply_impl(ExprHandle const & args, ContHandle cont, Env & env)
    {
        shared_ptr<AndWorker> thunk
            = make_shared<AndWorker>(args, cont, env, 0u);
        WorkerHandle start = thunk;
        GlobalWorkQueue.put(start);
    }
}

AndWorker::AndWorker(ExprHandle const & args,
                     ContHandle cont,
                     Env env,
                     size_t n)
    : args(args)
    , cont(cont)
    , env(env)
    , n(n)
{
}

void AndWorker::run()
{
    if ( ! args->isList() ) {
        stringstream s;
        s << "And expects a list of forms; got: " << args->toString();
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return;
    }

    size_t const len = args->length();
    if ( 0 == len ) {
        cont->run(ExpressionFactory::makeBoolean(true));
    }
    else if ( n == (len - 1) ) {
        args->nth(len-1)->eval(cont, env);
    }
    else {
        ExprHandle test = args->nth(n);

        ContHandle newCont = make_shared<AndCont>(args, cont, env, n+1);
        test->eval(newCont, env);
    }
}

AndCont::AndCont(ExprHandle const & args, ContHandle cont, Env & env, size_t n)
    : args(args)
    , cont(cont)
    , env(env)
    , n(n)
{
}

void AndCont::run(ExprHandle expr) const
{
    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( ! expr->truth() ) {
        cont->run(expr);
    }
    else {
        shared_ptr<AndWorker> thunk
            = make_shared<AndWorker>(args, cont, env, n);
        WorkerHandle start = thunk;
        GlobalWorkQueue.put(start);
    }
}
