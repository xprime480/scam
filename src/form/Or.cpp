
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
    extern void apply_impl(ScamExpr * args, Continuation * cont, Env env);
}

Or::Or()
    : SpecialForm("or")
{
}

Or * Or::makeInstance()
{
    static Or instance;
    return &instance;
}

void Or::apply(ScamExpr * args, Continuation * cont, Env env)
{
    apply_impl(args, cont, env);
}

namespace
{
    class OrWorker : public Worker
    {
    public:
        OrWorker(Continuation * cont, Env env, ScamExpr * args, size_t n);
        void run() override;

    private:
        ScamExpr * args;
        Continuation * cont;
        Env env;
        size_t n;
    };

    class OrCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        OrCont(ScamExpr * args, Continuation * cont, Env env, size_t n);

        static OrCont *
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
        workQueueHelper<OrWorker>(cont, env, args, pos);
    }
}

OrWorker::OrWorker(Continuation * cont,
                   Env env,
                   ScamExpr * args,
                   size_t n)
    : Worker("Or")
    , args(args)
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
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return;
    }

    size_t const len = args->length();
    if ( 0 == len ) {
        ScamExpr * rv = ExpressionFactory::makeBoolean(false);
        cont->run(rv);
    }
    else if ( n == (len - 1) ) {
        args->nthcar(len-1)->eval(cont, env);
    }
    else {
        ScamExpr * test = args->nthcar(n);

        Continuation * newCont =
            standardMemoryManager.make<OrCont>(args, cont, env, n+1);
        test->eval(newCont, env);
    }
}

OrCont::OrCont(ScamExpr * args, Continuation * cont, Env env, size_t n)
    : Continuation("Or")
    , args(args)
    , cont(cont)
    , env(env)
    , n(n)
{
}

OrCont *
OrCont::makeInstance(ScamExpr * args, Continuation * cont, Env env, size_t n)
{
  return new OrCont(args, cont, env, n);
}

void OrCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
    }
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
        workQueueHelper<OrWorker>(cont, env, args, n);
    }
}
