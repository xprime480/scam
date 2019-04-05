
#include "prim/Primitive.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    struct PrimWorkerData
    {
        PrimWorkerData(ScamExpr * args,
                       ContHandle original,
                       Env env,
                       Primitive * caller);

        ScamExpr * args;
        ContHandle original;
        ContHandle cont;
        Env env;
        Primitive * caller;

        void mapEval();
        void handleResult(ScamExpr * expr);
    };

    class  PrimWorker : public Worker
    {
    public:
        PrimWorker(ContHandle cont,
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

void Primitive::apply(ScamExpr * args, ContHandle cont, Env env)
{
    workQueueHelper<PrimWorker>(cont, env, args, this);
}

namespace
{
    PrimWorkerData::PrimWorkerData(ScamExpr * args,
                                   ContHandle original,
                                   Env env,
                                   Primitive * caller)
        : args(args)
        , original(original)
        , env(env)
        , caller(caller)
    {
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
    public:
        EvalContinuation(PrimWorkerData const & data);

        void run(ScamExpr * expr) override;

    private:
        PrimWorkerData data;
    };

    PrimWorker::PrimWorker(ContHandle cont,
                           Env env,
                           ScamExpr * args,
                           Primitive * caller)
        : Worker("Primitive")
        , data(args, cont, env, caller)
    {
        data.cont = make_shared<EvalContinuation>(data);
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

    void EvalContinuation::run(ScamExpr * expr)
    {
        Continuation::run(expr);
        data.handleResult(expr);
    }
}
