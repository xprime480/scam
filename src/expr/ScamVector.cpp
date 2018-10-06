#include "expr/ScamVector.hpp"

#include "Extractor.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    struct VectorWorkerData;

    class  VectorWorker : public Worker
    {
    public:
        VectorWorker(vector<shared_ptr<ScamExpr>> const & forms,
                     shared_ptr<Continuation> cont,
                     Env & env);

        VectorWorker(shared_ptr<VectorWorkerData> data);

        void run() override;

    private:
        shared_ptr<VectorWorkerData> data;
    };
}

ScamVector::ScamVector(vector<shared_ptr<ScamExpr>> const & elts)
    : elts(elts)
{
}

string ScamVector::toString() const
{
    stringstream s;
    string sep { "" };

    s << "[";
    for ( auto const & e : elts ) {
        s << sep << e->toString();
        sep = " ";
    }
    s << "]";

    return s.str();
}

void ScamVector::eval(shared_ptr<Continuation> cont, Env & env)
{
    shared_ptr<VectorWorker> thunk = make_shared<VectorWorker>(elts, cont, env);
    WorkerHandle start = thunk;
    GlobalWorkQueue.put(start);
}

bool ScamVector::isVector() const
{
    return true;
}

size_t ScamVector::length() const
{
    return elts.size();
}

shared_ptr<ScamExpr> ScamVector::nth(size_t n) const
{
    if ( n >= length() ) {
        stringstream s;
        s << "Requested index " << n
          << " of a " << length() << "-element vector";
        return ExpressionFactory::makeError(s.str());
    }

    return elts[n];
}

shared_ptr<ScamExpr> ScamVector::clone()
{
    return ExpressionFactory::makeVector(elts);
}

namespace
{
    struct VectorWorkerData
    {
        VectorWorkerData(vector<shared_ptr<ScamExpr>> const & forms,
                         shared_ptr<Continuation> original,
                         Env & env)
            : forms(forms)
            , original(original)
            , env(env)
            , evaled(forms.size(), ExpressionFactory::makeNull())
            , index(0u)
        {
        }

        vector<shared_ptr<ScamExpr>> forms;
        shared_ptr<Continuation> original;
        shared_ptr<Continuation> cont;
        Env & env;

        vector<shared_ptr<ScamExpr>> evaled;
        size_t index;
    };

    class EvalContinuation : public Continuation
    {
    public:
        EvalContinuation(shared_ptr<VectorWorkerData> data);

        void run(shared_ptr<ScamExpr> expr) const override;

    private:
        shared_ptr<VectorWorkerData> data;
    };

    VectorWorker::VectorWorker(vector<shared_ptr<ScamExpr>> const & forms,
                               shared_ptr<Continuation> cont,
                               Env & env)
        : data(make_shared<VectorWorkerData>(forms,
                                             cont,
                                             env))
    {
        data->cont = make_shared<EvalContinuation>(data);
    }

    VectorWorker::VectorWorker(shared_ptr<VectorWorkerData> data)
        : data(data)
    {
    }

    void VectorWorker::run()
    {
        if ( data->index >= data->forms.size() ) {
            shared_ptr<ScamExpr> value
                = ExpressionFactory::makeVector(data->evaled);
            data->original->run(value);
        }
        else {
            shared_ptr<ScamExpr> expr = data->forms[data->index];
            expr->eval(data->cont, data->env);
        }
    }

    EvalContinuation::EvalContinuation(shared_ptr<VectorWorkerData> data)
        : data(data)
    {
    }

    void EvalContinuation::run(shared_ptr<ScamExpr> expr) const
    {
        if ( expr->error() ) {
            data->original->run(expr);
            return;
        }

        data->evaled[data->index] = expr;
        data->index += 1;

        shared_ptr<VectorWorker> nextThunk = make_shared<VectorWorker>(data);
        WorkerHandle next = nextThunk;
        GlobalWorkQueue.put(next);
    }
}
