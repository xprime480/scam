
#include "prim/Primitive.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "util/MemoryManager.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    struct PrimWorkerData
    {
        PrimWorkerData(ScamExpr * args,
                       Continuation * original,
                       Env env,
                       Primitive * caller);

        ScamExpr * args;
        Continuation * original;
        Continuation * cont;
        Env env;
        Primitive * caller;

        void mark() const;

        void mapEval();
        void handleResult(ScamExpr * expr);
    };

    class  PrimWorker : public Worker
    {
    public:
        PrimWorker(Continuation * cont,
                   Env env,
                   ScamExpr * args,
                   Primitive * caller);

        void run() override;

    private:
        PrimWorkerData data;
    };
}

Primitive::Primitive(string const & name)
    : name(name)
{
}

string Primitive::toString() const
{
    stringstream s;
    s << "Primitive " << name;
    return s.str();
}

bool Primitive::hasApply() const
{
    return true;
}

void Primitive::apply(ScamExpr * args, Continuation * cont, Env env)
{
    workQueueHelper<PrimWorker>(cont, env, args, this);
}

namespace
{
    PrimWorkerData::PrimWorkerData(ScamExpr * args,
                                   Continuation * original,
                                   Env env,
                                   Primitive * caller)
        : args(args)
        , original(original)
        , cont(nullptr)
        , env(env)
        , caller(caller)
    {
    }

    void PrimWorkerData::mark() const
    {
        args->mark();
        original->mark();
        if ( cont ) { cont->mark(); };
    }

    void PrimWorkerData::mapEval()
    {
        args->mapEval(cont, env);
    }

    void PrimWorkerData::handleResult(ScamExpr * expr)
    {
        if ( expr->error() ) {
            original->run(expr);
        }
        else {
            caller->applyArgs(expr, original);
        }
    }

    class EvalContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        EvalContinuation(PrimWorkerData const & data);

        static EvalContinuation * makeInstance(PrimWorkerData const & data);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    private:
        PrimWorkerData data;
    };

    PrimWorker::PrimWorker(Continuation * cont,
                           Env env,
                           ScamExpr * args,
                           Primitive * caller)
        : Worker("Primitive")
        , data(args, cont, env, caller)
    {
        data.cont = standardMemoryManager.make<EvalContinuation>(data);
    }

    void PrimWorker::run()
    {
        Worker::run();
        data.mapEval();
    }

    EvalContinuation::EvalContinuation(PrimWorkerData const & data)
        : Continuation("Primitive Eval")
        , data(data.args, data.original, data.env, data.caller)
    {
    }

    EvalContinuation *
    EvalContinuation::makeInstance(PrimWorkerData const & data)
    {
        return new EvalContinuation(data);
    }

    void EvalContinuation::mark() const
    {
        if ( ! isMarked() ) {
            Continuation::mark();
            data.mark();
        }
    }

    void EvalContinuation::run(ScamExpr * expr)
    {
        Continuation::run(expr);
        data.handleResult(expr);
    }
}
