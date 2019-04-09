
#include "form/If.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    class IfWorker : public Worker
    {
    public:
        IfWorker(Continuation * cont, Env env, ScamExpr * args);
        void run() override;

    private:
        ScamExpr * args;
        Continuation * cont;
        Env env;
    };
}

If::If()
    : SpecialForm("if")
{
}

If * If::makeInstance()
{
    static If instance;
    return &instance;
}

void If::apply(ScamExpr * args, Continuation * cont, Env env)
{
    workQueueHelper<IfWorker>(cont, env, args);
}

namespace
{
    class IfCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        IfCont(ScamExpr * args, Continuation * cont, Env env);

        static IfCont *
        makeInstance(ScamExpr * args, Continuation * cont, Env env);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    private:
        ScamExpr * args;
        Continuation * cont;
        Env env;
    };
}

IfWorker::IfWorker(Continuation * cont, Env env, ScamExpr * args)
    : Worker("If")
    , args(args)
    , cont(cont)
    , env(env)
{
}

void IfWorker::run()
{
    Worker::run();

    if ( ! args->isList() || args->length() < 2 || args->length() > 3 ) {
        stringstream s;
        s << "If expects 2 or 3 forms; got: " << args->toString();
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }
    else {
        Continuation * newCont =
            standardMemoryManager.make<IfCont>(args, cont, env);
        ScamExpr * test = args->nthcar(0);

        test->eval(newCont, env);
    }
}

IfCont::IfCont(ScamExpr * args, Continuation * cont, Env env)
    : Continuation("If")
    , args(args)
    , cont(cont)
    , env(env)
{
}

IfCont * IfCont::makeInstance(ScamExpr * args, Continuation * cont, Env env)
{
    return new IfCont(args, cont, env);
}

void IfCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
    }
}

void IfCont::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( expr->truth() ) {
        args->nthcar(1)->eval(cont, env);
    }
    else if ( args->length() > 2 ) {
        args->nthcar(2)->eval(cont, env);
    }
    else {
        cont->run(ExpressionFactory::makeNil());
    }
}
