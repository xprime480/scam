
#include "form/If.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    class IfWorker : public Worker
    {
    public:
        IfWorker(ContHandle cont, Env env, ScamExpr * args);
        void run() override;

    private:
        ExprHandle args;
        ContHandle cont;
        Env env;
    };
}

If::If()
    : SpecialForm("if")
{
}

void If::apply(ScamExpr * args, ContHandle cont, Env env)
{
    workQueueHelper<IfWorker>(cont, env, args);
}

namespace
{
    class IfCont : public Continuation
    {
    public:
        IfCont(ScamExpr * args, ContHandle cont, Env env);
        void run(ScamExpr * expr) override;

    private:
        ExprHandle args;
        ContHandle cont;
        Env env;
    };
}

IfWorker::IfWorker(ContHandle cont, Env env, ScamExpr * args)
    : Worker("If")
    , args(args->clone())
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
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err.get());
    }
    else {
        ContHandle newCont = make_shared<IfCont>(args.get(), cont, env);
        ExprHandle test = args->nthcar(0);
        test->eval(newCont, env);
    }
}

IfCont::IfCont(ScamExpr * args, ContHandle cont, Env env)
    : Continuation("If")
    , args(args->clone())
    , cont(cont)
    , env(env)
{
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
        cont->run(ExpressionFactory::makeNil().get());
    }
}
