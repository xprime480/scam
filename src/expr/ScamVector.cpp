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
    struct VectorWorkerData
    {
        VectorWorkerData(Env & env)
            : env(env)
        {
        }

        vector<shared_ptr<ScamExpr>> forms;
        std::shared_ptr<Continuation> original;
        std::shared_ptr<Continuation> cont;
        Env & env;

        vector<shared_ptr<ScamExpr>> evaled;
    };

    class EvalContinuation : public Continuation
    {
    public:
        EvalContinuation(shared_ptr<VectorWorkerData> data);

        void run(shared_ptr<ScamExpr> expr) const override;

    private:
        shared_ptr<VectorWorkerData> data;
    };

    class  VectorWorker : public Worker
    {
    public:
        VectorWorker(vector<shared_ptr<ScamExpr>> const & forms,
                     std::shared_ptr<Continuation> cont,
                     Env & env)
            : data(make_shared<VectorWorkerData>(env))
        {
            data->forms = forms;
            data->original = cont;
            data->cont = make_shared<EvalContinuation>(data);
        }

        VectorWorker(shared_ptr<VectorWorkerData> data)
            : data(data)
        {
        }

        VectorWorker(VectorWorker const &) = default;
        VectorWorker & operator=(VectorWorker const &) = default;
        VectorWorker(VectorWorker &&) = delete;
        VectorWorker & operator=(VectorWorker &&) = delete;

        void run() override
        {
            if ( data->forms.empty() ) {
                shared_ptr<ScamExpr> value
                    = ExpressionFactory::makeVector(data->evaled);
                data->original->run(value);
            }
            else {
                shared_ptr<ScamExpr> expr = data->forms.front();
                data->forms.erase(data->forms.begin());
                expr->eval(data->cont, data->env);
            }
        }

    private:
        shared_ptr<VectorWorkerData> data;
    };

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

        data->evaled.push_back(expr);

        shared_ptr<VectorWorker> nextThunk = make_shared<VectorWorker>(data);
        WorkerHandle next = nextThunk;
        GlobalWorkQueue.put(next);
    }

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

void ScamVector::eval(std::shared_ptr<Continuation> cont, Env & env)
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
