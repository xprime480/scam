
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
        ConsWorker(shared_ptr<ScamExpr> & car,
                   shared_ptr<ScamExpr> & cdr,
                   std::shared_ptr<Continuation> cont,
                   Env & env);

        ConsWorker(shared_ptr<ConsWorkerData> data);

        void run() override;

    private:
        shared_ptr<ConsWorkerData> data;
    };
}

ScamCons::ScamCons(shared_ptr<ScamExpr> const & car,
                   shared_ptr<ScamExpr> const & cdr)
    : car(car)
    , cdr(cdr)
{
}

string ScamCons::toString() const
{
    stringstream s;
    s << "(";
    s << car->toString();
    shared_ptr<ScamExpr> next = cdr;
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

void ScamCons::eval(std::shared_ptr<Continuation> cont, Env & env)
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

shared_ptr<ScamExpr> ScamCons::getCar() const
{
    return car;
}

shared_ptr<ScamExpr> ScamCons::getCdr() const
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

std::shared_ptr<ScamExpr> ScamCons::nth(size_t n) const
{
    auto f = [=] () -> std::shared_ptr<ScamExpr> {
        stringstream s;
        s << "Index " << n << " requested for " << toString();
        return ExpressionFactory::makeError(s.str());
    };

    std::shared_ptr<ScamExpr> rv;

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

shared_ptr<ScamExpr> ScamCons::clone()
{
    return ExpressionFactory::makeCons(car, cdr);
}

namespace
{
    class Evaluator : public Continuation
    {
    public:
        Evaluator(shared_ptr<ScamExpr> args,
                  std::shared_ptr<Continuation> cont,
                  Env env)
            : args(args)
            , cont(cont)
            , env(env)
        {
        }

        void run(shared_ptr<ScamExpr> e) const override
        {
            e->apply(args, cont, env);
        }

    private:
        mutable shared_ptr<ScamExpr> args;
        mutable std::shared_ptr<Continuation> cont;
        mutable Env env;
    };
}

namespace
{
    struct ConsWorkerData
    {
        ConsWorkerData(shared_ptr<ScamExpr> car,
                       shared_ptr<ScamExpr> cdr,
                       std::shared_ptr<Continuation> original,
                       Env & env)
            : car(car)
            , cdr(cdr)
            , original(original)
            , env(env)
        {
        }

        shared_ptr<ScamExpr> car;
        shared_ptr<ScamExpr> cdr;
        shared_ptr<Continuation> original;
        shared_ptr<Continuation> cont;
        Env & env;
    };

    class EvalContinuation : public Continuation
    {
    public:
        EvalContinuation(shared_ptr<ConsWorkerData> data);

        void run(shared_ptr<ScamExpr> expr) const override;

    private:
        shared_ptr<ConsWorkerData> data;
    };

    ConsWorker::ConsWorker(shared_ptr<ScamExpr> & car,
                           shared_ptr<ScamExpr> & cdr,
                           std::shared_ptr<Continuation> cont,
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

    void EvalContinuation::run(shared_ptr<ScamExpr> expr) const
    {
        if ( expr->error() ) {
            data->original->run(expr);
        }
        else {
            expr->apply(data->cdr, data->original, data->env);
        }
    }
}
