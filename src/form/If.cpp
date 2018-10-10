
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
        IfWorker(ContHandle cont, Env & env, ExprHandle const & args);
        void run() override;

    private:
        ExprHandle const & args;
        ContHandle cont;
        Env & env;
    };
}

If::If()
    : SpecialForm("if")
{
}

void If::apply(ExprHandle const & args, ContHandle cont, Env & env)
{
    workQueueHelper<IfWorker>(cont, env, args);
}

ExprHandle If::clone() const
{
    return ExpressionFactory::makeForm<If>();
}

namespace
{
    class IfCont : public Continuation
    {
    public:
        IfCont(ExprHandle const & args, ContHandle cont, Env & env);
        void run(ExprHandle expr) const override;

    private:
        ExprHandle const & args;
        ContHandle cont;
        Env & env;
    };
}

IfWorker::IfWorker(ContHandle cont, Env & env, ExprHandle const & args)
    : args(args)
    , cont(cont)
    , env(env)
{
}

void IfWorker::run()
{
    if ( ! args->isList() || args->length() < 2 || args->length() > 3 ) {
        stringstream s;
        s << "If expects 2 or 3 forms; got: " << args->toString();
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }
    else {
        ContHandle newCont = make_shared<IfCont>(args, cont, env);
        ExprHandle test = args->nth(0);
        test->eval(newCont, env);
    }
}

IfCont::IfCont(ExprHandle const & args, ContHandle cont, Env & env)
    : args(args)
    , cont(cont)
    , env(env)
{
}

void IfCont::run(ExprHandle expr) const
{
    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( expr->truth() ) {
        args->nth(1)->eval(cont, env);
    }
    else if ( args->length() > 2 ) {
        args->nth(2)->eval(cont, env);
    }
    else {
        cont->run(ExpressionFactory::makeNil());
    }
}
