#include "expr/ScamVector.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void eval_vector(ContHandle cont, Env env, ExprVec const & elts);
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

void ScamVector::eval(ContHandle cont, Env env)
{
    eval_vector(cont, env, elts);
}

bool ScamVector::isVector() const
{
    return true;
}

size_t ScamVector::length() const
{
    return elts.size();
}

ExprHandle ScamVector::nthcar(size_t n) const
{
    if ( n >= length() ) {
        stringstream s;
        s << "Requested index " << n
          << " of a " << length() << "-element vector";
        return ExpressionFactory::makeError(s.str());
    }

    return elts[n];
}

namespace
{
    class VectorCont : public Continuation
    {
    public:
        VectorCont(ExprVec const & forms,
                   ExprVec const & evaled,
                   ContHandle original,
                   Env env);

        void run(ScamExpr * expr) override;

    private:
        ExprVec forms;
        ExprVec evaled;
        ContHandle original;
        Env env;
    };

    class  VectorWorker : public Worker
    {
    public:
        VectorWorker(ContHandle cont,
                     Env env,
                     ExprVec const & forms);

        VectorWorker(ContHandle cont,
                     Env env,
                     ExprVec const & forms,
                     ExprVec const & evaled);

        void run() override;

    private:
        ExprVec    forms;
        ExprVec    evaled;
        ContHandle original;
        Env        env;
    };

    VectorWorker::VectorWorker(ContHandle cont,
                               Env env,
                               ExprVec const & forms)
        : Worker("Vector")
        , forms(forms)
        , original(cont)
        , env(env)
    {
    }

    VectorWorker::VectorWorker(ContHandle cont,
                               Env env,
                               ExprVec const & forms,
                               ExprVec const & evaled)
        : Worker("Vector 2")
        , forms(forms)
        , evaled(evaled)
        , original(cont)
        , env(env)
    {
    }

    void VectorWorker::run()
    {
        Worker::run();

        if ( forms.size() == evaled.size() ) {
            ExprHandle value = ExpressionFactory::makeVector(evaled);
            original->run(value.get());
        }
        else {
            size_t index = evaled.size();
            ExprHandle expr = forms[index];
            ContHandle cont
                = make_shared<VectorCont>(forms, evaled, original, env);
            expr->eval(cont, env);
        }
    }

    VectorCont::VectorCont(ExprVec const & forms,
                           ExprVec const & evaled,
                           ContHandle original,
                           Env env)
        : Continuation("Vector")
        , forms(forms)
        , evaled(evaled)
        , original(original)
        , env(env)
    {
    }

    void VectorCont::run(ScamExpr * expr)
    {
        Continuation::run(expr);

        if ( expr->error() ) {
            original->run(expr);
        }
        else {
            ExprVec e2(evaled);
            e2.push_back(expr->clone());
            workQueueHelper<VectorWorker>(original, env, forms, e2);
        }
    }

    void eval_vector(ContHandle cont, Env env, ExprVec const & elts)
    {
        workQueueHelper<VectorWorker>(cont, env, elts);
    }
}
