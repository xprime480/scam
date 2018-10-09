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
        VectorWorker(ExprVec const & forms, ContHandle cont, Env & env);

        VectorWorker(shared_ptr<VectorWorkerData> data);

        void run() override;

    private:
        shared_ptr<VectorWorkerData> data;
    };
}

ScamVector::ScamVector(ExprVec const & elts)
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

void ScamVector::eval(ContHandle cont, Env & env)
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

ExprHandle ScamVector::nth(size_t n) const
{
    if ( n >= length() ) {
        stringstream s;
        s << "Requested index " << n
          << " of a " << length() << "-element vector";
        return ExpressionFactory::makeError(s.str());
    }

    return elts[n];
}

ExprHandle ScamVector::clone() const
{
    return ExpressionFactory::makeVector(elts);
}

namespace
{
    struct VectorWorkerData
    {
        VectorWorkerData(ExprVec const & forms, ContHandle original, Env & env)
            : forms(forms)
            , original(original)
            , env(env)
            , evaled(forms.size(), ExpressionFactory::makeNull())
            , index(0u)
        {
        }

        ExprVec forms;
        ContHandle original;
        ContHandle cont;
        Env & env;

        ExprVec evaled;
        size_t index;
    };

    class EvalContinuation : public Continuation
    {
    public:
        EvalContinuation(shared_ptr<VectorWorkerData> data);

        void run(ExprHandle expr) const override;

    private:
        shared_ptr<VectorWorkerData> data;
    };

    VectorWorker::VectorWorker(ExprVec const & forms,
                               ContHandle cont,
                               Env & env)
        : data(make_shared<VectorWorkerData>(forms, cont, env))
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
            ExprHandle value = ExpressionFactory::makeVector(data->evaled);
            data->original->run(value);
        }
        else {
            ExprHandle expr = data->forms[data->index];
            expr->eval(data->cont, data->env);
        }
    }

    EvalContinuation::EvalContinuation(shared_ptr<VectorWorkerData> data)
        : data(data)
    {
    }

    void EvalContinuation::run(ExprHandle expr) const
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
