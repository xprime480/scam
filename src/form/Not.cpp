
#include "form/Not.hpp"

#include "Continuation.hpp"
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

Not::Not()
    : SpecialForm("not")
{
}

void Not::apply(ScamExpr * args, ContHandle cont, Env env)
{
    apply_impl(args, cont, env);
}

namespace
{
    class NotWorker : public Worker
    {
    public:
        NotWorker(ContHandle cont, Env env, ScamExpr * args);
        void run() override;

    private:
        ExprHandle args;
        ContHandle cont;
        Env env;
    };

    class NotCont : public Continuation
    {
    public:
        NotCont(ContHandle cont);
        void run(ScamExpr * expr) override;

    private:
        ContHandle cont;
    };

    void apply_impl(ScamExpr * args, ContHandle cont, Env env)
    {
        workQueueHelper<NotWorker>(cont, env, args);
    }
}

NotWorker::NotWorker(ContHandle cont, Env env, ScamExpr * args)
    : Worker("Not")
    , args(args->clone())
    , cont(cont)
    , env(env)
{
}

void NotWorker::run()
{
    Worker::run();

    if ( ! args->isList() || 1u != args->length() ) {
        stringstream s;
        s << "Not expects 1 form; got: " << args->toString();
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err.get());
    }
    else {
        ContHandle newCont = make_shared<NotCont>(cont);
        ExprHandle test = args->nth(0);
        test->eval(newCont, env);
    }
}

NotCont::NotCont(ContHandle cont)
    : Continuation("Not")
    , cont(cont)
{
}

void NotCont::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else {
        cont->run(ExpressionFactory::makeBoolean(! expr->truth()).get());
    }
}
