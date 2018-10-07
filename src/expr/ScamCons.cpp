
#include "expr/ScamCons.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    struct ConsWorkerData;

    class  ConsWorker : public Worker
    {
    public:
        ConsWorker(ExprHandle & car,
                   ExprHandle & cdr,
                   ContHandle cont,
                   Env & env);

        ConsWorker(shared_ptr<ConsWorkerData> data);

        void run() override;

    private:
        shared_ptr<ConsWorkerData> data;
    };
}

ScamCons::ScamCons(ExprHandle const & car, ExprHandle const & cdr)
    : car(car)
    , cdr(cdr)
{
}

string ScamCons::toString() const
{
    stringstream s;
    s << "(";
    s << car->toString();
    ExprHandle next = cdr;
    while ( ! next->isNil() ) {
        if ( next->isCons() ) {
            s << " " << next->getCar()->toString();
            next = next->getCdr();
        }
        else {
            s << " . " << next->toString();
            break;
        }
    }
    s << ")";

    return s.str();
}

void ScamCons::eval(ContHandle cont, Env & env)
{
    shared_ptr<ConsWorker> thunk = make_shared<ConsWorker>(car, cdr, cont, env);
    WorkerHandle start = thunk;
    GlobalWorkQueue.put(start);
}

bool ScamCons::isCons() const
{
    return true;
}

bool ScamCons::isList() const
{
    if ( cdr->isNil() ) {
        return true;
    }
    if ( ! cdr->isCons() ) {
        return false;
    }
    return cdr->isList();
}

ExprHandle ScamCons::getCar() const
{
    return car;
}

ExprHandle ScamCons::getCdr() const
{
    return cdr;
}

size_t ScamCons::length() const
{
    size_t len = 1;
    if ( cdr->isCons() ) {
        len += cdr->length();
    }
    else if ( ! cdr->isNil() ) {
        len += 1;
    }

    return len;
}

ExprHandle ScamCons::nth(size_t n) const
{
    auto f = [=] () -> ExprHandle {
        stringstream s;
        s << "Index " << n << " requested for " << toString();
        return ExpressionFactory::makeError(s.str());
    };

    ExprHandle rv;

    if ( 0 == n ) {
        rv = car;
    }
    else if ( cdr->isCons() ) {
        rv = cdr->nth(n-1);
        if ( rv->error() ) {
            rv = f();
        }
    }
    else if ( cdr->isNil() || n > 1 ) {
        rv = f();
    }
    else {
        rv = cdr;
    }

    return rv;
}

ExprHandle ScamCons::clone()
{
    return ExpressionFactory::makeCons(car, cdr);
}

namespace
{
    class Evaluator : public Continuation
    {
    public:
        Evaluator(ExprHandle args, ContHandle cont, Env env)
            : args(args)
            , cont(cont)
            , env(env)
        {
        }

        void run(ExprHandle e) const override
        {
            e->apply(args, cont, env);
        }

    private:
        mutable ExprHandle args;
        mutable ContHandle cont;
        mutable Env env;
    };
}

namespace
{
    struct ConsWorkerData
    {
        ConsWorkerData(ExprHandle car,
                       ExprHandle cdr,
                       ContHandle original,
                       Env & env)
            : car(car)
            , cdr(cdr)
            , original(original)
            , env(env)
        {
        }

        ExprHandle car;
        ExprHandle cdr;
        ContHandle original;
        ContHandle cont;
        Env & env;
    };

    class EvalContinuation : public Continuation
    {
    public:
        EvalContinuation(shared_ptr<ConsWorkerData> data);

        void run(ExprHandle expr) const override;

    private:
        shared_ptr<ConsWorkerData> data;
    };

    ConsWorker::ConsWorker(ExprHandle & car,
                           ExprHandle & cdr,
                           ContHandle cont,
                           Env & env)
        : data(make_shared<ConsWorkerData>(car, cdr, cont, env))
    {
        data->cont = make_shared<EvalContinuation>(data);
    }

    ConsWorker::ConsWorker(shared_ptr<ConsWorkerData> data)
        : data(data)
    {
    }

    void ConsWorker::run()
    {
        data->car->eval(data->cont, data->env);
    }

    EvalContinuation::EvalContinuation(shared_ptr<ConsWorkerData> data)
        : data(data)
    {
    }

    void EvalContinuation::run(ExprHandle expr) const
    {
        if ( expr->error() ) {
            data->original->run(expr);
        }
        else {
            expr->apply(data->cdr, data->original, data->env);
        }
    }
}
