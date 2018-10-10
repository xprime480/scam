
#include "form/Not.hpp"

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
    extern void apply_impl(ExprHandle const & args, ContHandle cont, Env & env);
}

Not::Not()
    : SpecialForm("not")
{
}

void Not::apply(ExprHandle const & args, ContHandle cont, Env & env)
{
    apply_impl(args, cont, env);
}

ExprHandle Not::clone() const
{
    return ExpressionFactory::makeForm<Not>();
}

namespace
{
    class NotWorker : public Worker
    {
    public:
        NotWorker(ContHandle cont, Env & env, ExprHandle const & args);
        void run() override;

    private:
        ExprHandle const & args;
        ContHandle cont;
        Env & env;
    };

    class NotCont : public Continuation
    {
    public:
        NotCont(ContHandle cont);
        void run(ExprHandle expr) const override;

    private:
        ContHandle cont;
    };

    void apply_impl(ExprHandle const & args, ContHandle cont, Env & env)
    {
        workQueueHelper<NotWorker>(cont, env, args);
    }
}

NotWorker::NotWorker(ContHandle cont, Env & env, ExprHandle const & args)
    : args(args)
    , cont(cont)
    , env(env)
{
}

void NotWorker::run()
{
    if ( ! args->isList() || 1u != args->length() ) {
        stringstream s;
        s << "Not expects 1 form; got: " << args->toString();
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }
    else {
        ContHandle newCont = make_shared<NotCont>(cont);
        ExprHandle test = args->nth(0);
        test->eval(newCont, env);
    }
}

NotCont::NotCont(ContHandle cont)
    : cont(cont)
{
}

void NotCont::run(ExprHandle expr) const
{
    if ( expr->error() ) {
        cont->run(expr);
    }
    else {
        cont->run(ExpressionFactory::makeBoolean(! expr->truth()));
    }
}
