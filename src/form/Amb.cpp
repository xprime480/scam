
#include "form/Amb.hpp"

#include "Backtracker.hpp"
#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void
    do_apply(ScamExpr * args, ContHandle cont, Env env, ScamEngine * engine);
}

Amb::Amb(ScamEngine * engine)
    : SpecialForm("amb")
    , engine(engine)
{
}

void Amb::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(args, cont, env, engine);
}

namespace
{
    class AmbWorker : public Worker
    {
    public:
        AmbWorker(ScamExpr * args,
                  ContHandle cont,
                  Env env,
                  ScamEngine * engine,
                  BacktrackHandle backtracker);

        void run() override;

    private:
        ExprHandle args;
        ContHandle original;
        Env env;
        ScamEngine * engine;
        BacktrackHandle backtracker;
    };

    class AmbBacktracker : public Backtracker
    {
    public:
        AmbBacktracker(ScamExpr * args,
                       ContHandle original,
                       Env env,
                       ScamEngine * engine,
                       BacktrackHandle backtracker)
            : Backtracker("AmbBacktracker")
            , args(args->clone())
            , original(original)
            , env(env)
            , engine(engine)
            , backtracker(backtracker)
        {
        }

        void run(ContHandle cont) override
        {
            Backtracker::run(cont);

            if ( 0 == args->length() ) {
                if ( nullptr == backtracker.get() ) {
                    static const ExprHandle nomore =
                        ExpressionFactory::makeError("No more choices");
                    cont->run(nomore.get());
                }
                else {
                    backtracker->run(cont);
                }
            }
            else {
                ScamExpr * argp = args.get();
                workQueueHelper<AmbWorker>(argp,
                                           original,
                                           env,
                                           engine,
                                           backtracker);
            }
        }

    private:
        ExprHandle      args;
        ContHandle      original;
        Env             env;
        ScamEngine    * engine;
        BacktrackHandle backtracker;
    };

    class AmbContinuation : public Continuation
    {
    public:
        AmbContinuation(ScamExpr * args,
                        ContHandle original,
                        Env env,
                        ScamEngine * engine,
                        BacktrackHandle backtracker)
            : Continuation("AmbContinuation")
            , args(args->clone())
            , original(original)
            , env(env)
            , engine(engine)
            , backtracker(backtracker)
        {
        }

        void run(ScamExpr * expr) override
        {
            shared_ptr<Backtracker> bt =
                make_shared<AmbBacktracker>(args.get(),
                                            original,
                                            env,
                                            engine,
                                            backtracker);
            engine->setBacktracker(bt);
            original->run(expr);
        }

    private:
        ExprHandle      args;
        ContHandle      original;
        Env             env;
        ScamEngine    * engine;
        BacktrackHandle backtracker;
    };

    AmbWorker::AmbWorker(ScamExpr * args,
                         ContHandle cont,
                         Env env,
                         ScamEngine * engine,
                         BacktrackHandle backtracker
                         )
            : Worker("AmbWorker")
            , args(args->clone())
            , original(cont)
            , env(env)
            , engine(engine)
            , backtracker(backtracker)
        {
        }

    void AmbWorker::run()
    {
        if ( 0 == args->length() ) {
            if ( backtracker.get() ) {
                backtracker->run(original);
            }
            else {
                static const ExprHandle nomore =
                    ExpressionFactory::makeError("No more choices");
                original->run(nomore.get());
            }
        }
        else {
            ExprHandle first = args->nthcar(0);
            ExprHandle rest = args->nthcdr(0);

            ContHandle cont =
                make_shared<AmbContinuation>(rest.get(),
                                             original,
                                             env,
                                             engine,
                                             backtracker);
            first->eval(cont, env);
        }
    }

    void
    do_apply(ScamExpr * args, ContHandle cont, Env env, ScamEngine * engine)
    {
        BacktrackHandle backtracker = engine->getBacktracker();
        workQueueHelper<AmbWorker>(args,
                                   cont,
                                   env,
                                   engine,
                                   backtracker);
    }
}
