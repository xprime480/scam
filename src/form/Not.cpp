
#include "form/Not.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace scam
{
    class MemoryManager;
}

namespace
{
    extern void apply_impl(ScamExpr * args, Continuation * cont, Env * env);
}

Not::Not()
    : SpecialForm("not")
{
}

Not * Not::makeInstance()
{
    static Not instance;
    return &instance;
}

void Not::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    apply_impl(args, cont, env);
}

namespace
{
    class NotWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        NotWorker(Continuation * cont, Env * env, ScamExpr * args);

        static NotWorker *
        makeInstance(Continuation * cont, Env * env, ScamExpr * args);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr * args;
        Continuation * cont;
        Env * env;
    };

    class NotCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        NotCont(Continuation * cont);
        static NotCont * makeInstance(Continuation * cont);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    private:
        Continuation * cont;
    };

    void apply_impl(ScamExpr * args, Continuation * cont, Env * env)
    {
        workQueueHelper<NotWorker>(cont, env, args);
    }
}

NotWorker::NotWorker(Continuation * cont, Env * env, ScamExpr * args)
    : Worker("Not")
    , args(args)
    , cont(cont)
    , env(env)
{
}

NotWorker *
NotWorker::makeInstance(Continuation * cont, Env * env, ScamExpr * args)
{
    return new NotWorker(cont, env, args);
}

void NotWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void NotWorker::run()
{
    Worker::run();

    if ( ! args->isList() || 1u != args->length() ) {
        stringstream s;
        s << "Not expects 1 form; got: " << args->toString();
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }
    else {
        Continuation * newCont = standardMemoryManager.make<NotCont>(cont);
        ScamExpr * test = args->nthcar(0);
        test->eval(newCont, env);
    }
}

NotCont::NotCont(Continuation * cont)
    : Continuation("Not")
    , cont(cont)
{
}

NotCont * NotCont::makeInstance(Continuation * cont)
{
    return new NotCont(cont);
}

void NotCont::mark() const
{
  if ( ! isMarked() ) {
      Continuation::mark();
      cont->mark();
  }
}

void NotCont::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else {
        ScamExpr * rv = ExpressionFactory::makeBoolean(! expr->truth());
        cont->run(rv);
    }
}
