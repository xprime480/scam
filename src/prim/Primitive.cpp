
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
        PrimWorker(ExprHandle const & args,
                   ContHandle cont,
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

void Primitive::apply(ExprHandle const & args, ContHandle cont, Env & env)
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
        PrimWorkerData(ExprHandle args,
                       ContHandle original,
                       Env & env,
                       Primitive * caller)
            : args(args)
            , original(original)
            , env(env)
            , caller(caller)
        {
        }

        ExprHandle args;
        ContHandle original;
        ContHandle cont;
        Env & env;
        Primitive * caller;

        void mapEval()
        {
            args->mapEval(cont, env);
        }

        void handleResult(ExprHandle expr)
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

        void run(ExprHandle expr) const override;

    private:
        shared_ptr<PrimWorkerData> data;
    };

    PrimWorker::PrimWorker(ExprHandle const & args,
                           ContHandle cont,
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

    void EvalContinuation::run(ExprHandle expr) const
    {
        data->handleResult(expr);
    }
}
