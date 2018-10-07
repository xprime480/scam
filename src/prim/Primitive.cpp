
#include "prim/Primitive.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    struct PrimWorkerData;

    class  PrimWorker : public Worker
    {
    public:
        PrimWorker(shared_ptr<ScamExpr> const & args,
                   std::shared_ptr<Continuation> cont,
                   Env & env,
                   Primitive * caller);

        PrimWorker(shared_ptr<PrimWorkerData> data);

        void run() override;

    private:
        shared_ptr<PrimWorkerData> data;
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

void Primitive::apply(std::shared_ptr<ScamExpr> const & args,
                      std::shared_ptr<Continuation> cont,
                      Env & env)
{
    shared_ptr<PrimWorker> thunk
        = make_shared<PrimWorker>(args, cont, env, this);
    WorkerHandle start = thunk;
    GlobalWorkQueue.put(start);
}

namespace
{
    struct PrimWorkerData
    {
        PrimWorkerData(shared_ptr<ScamExpr> args,
                       std::shared_ptr<Continuation> original,
                       Env & env,
                       Primitive * caller)
            : args(args)
            , original(original)
            , env(env)
            , caller(caller)
        {
        }

        shared_ptr<ScamExpr> args;
        shared_ptr<Continuation> original;
        shared_ptr<Continuation> cont;
        Env & env;
        Primitive * caller;

        void mapEval()
        {
            args->mapEval(cont, env);
        }

        void handleResult(shared_ptr<ScamExpr> expr)
        {
            if ( expr->error() ) {
                original->run(expr);
            }
            else {
                caller->applyArgs(expr, original);
            }
        }
    };

    class EvalContinuation : public Continuation
    {
    public:
        EvalContinuation(shared_ptr<PrimWorkerData> data);

        void run(shared_ptr<ScamExpr> expr) const override;

    private:
        shared_ptr<PrimWorkerData> data;
    };

    PrimWorker::PrimWorker(shared_ptr<ScamExpr> const & args,
                           std::shared_ptr<Continuation> cont,
                           Env & env,
                           Primitive * caller)
        : data(make_shared<PrimWorkerData>(args, cont, env, caller))
    {
        data->cont = make_shared<EvalContinuation>(data);
    }

    PrimWorker::PrimWorker(shared_ptr<PrimWorkerData> data)
        : data(data)
    {
    }

    void PrimWorker::run()
    {
        data->mapEval();
    }

    EvalContinuation::EvalContinuation(shared_ptr<PrimWorkerData> data)
        : data(data)
    {
    }

    void EvalContinuation::run(shared_ptr<ScamExpr> expr) const
    {
        data->handleResult(expr);
    }
}
