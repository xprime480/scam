
#include "expr/ScamClosure.hpp"

#include "Binder.hpp"
#include "Continuation.hpp"
#include "EvalWorker.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    void do_apply(ScamExpr * formals,
                  ScamExpr * forms,
                  Env capture,
                  ContHandle cont,
                  ScamExpr * args,
                  Env argEnv,
                  bool macrolike);
}

ScamClosure::ScamClosure(ScamExpr *formals,
                         ScamExpr * forms,
                         Env env,
                         bool macrolike)
    : formals(formals)
    , forms(forms)
    , env(env)
    , macrolike(macrolike)
{
}

ScamClosure * ScamClosure::makeInstance(ScamExpr *formals,
                                        ScamExpr * forms,
                                        Env env,
                                        bool macrolike)
{
    return new ScamClosure(formals, forms, env, macrolike);
}

void ScamClosure::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        if ( formals ) {
            formals->mark();
        }
        if ( forms ) {
            forms->mark();
        }
    }
}

string ScamClosure::toString() const
{
    stringstream s;
    s << "(";
    if ( macrolike ) {
        s << "macro ";
    }
    else {
        s << "lambda ";
    }
    s << formals->toString() << " " << forms->toString() << ")";
    return s.str();
}

bool ScamClosure::hasApply() const
{
    return true;
}

void ScamClosure::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(formals, forms, this->env, cont, args, env, macrolike);
}

bool ScamClosure::isProcedure() const
{
    return true;
}

ScamExpr * ScamClosure::withEnvUpdate(Env updated) const
{
    return ExpressionFactory::makeClosure(formals, forms, updated);
}

namespace
{
    class MacroEvalCont : public Continuation
    {
    public:
        MacroEvalCont(ContHandle cont, Env capture)
            : Continuation("macro eval")
            , cont(cont)
            , capture(capture)
        {
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);
            expr->eval(cont, capture);
        }

    private:
        ContHandle cont;
        Env        capture;
    };

    class ClosureBindCont : public Continuation
    {
    public:
        ClosureBindCont(ScamExpr * formals,
                        ScamExpr * forms,
                        Env capture,
                        ContHandle cont,
                        bool macrolike)
            : Continuation("proc - bind")
            , formals(formals)
            , forms(forms)
            , capture(capture)
            , cont(cont)
            , macrolike(macrolike)
        {
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);

            if ( expr->error() ) {
                cont->run(expr);
            }
            else if ( malformedActuals(expr) ) {
                /* do  nothing */
            }
            else if ( checkArgLength(expr) ) {
                finalize(expr);
            }
        }

    private:
        ScamExpr * formals;
        ScamExpr * forms;
        Env        capture;
        ContHandle cont;
        bool       macrolike;

        bool malformedActuals(ScamExpr * expr) const
        {
            if ( expr->isCons() || expr->isNil() || expr->isSymbol() ) {
                return false;
            }

            stringstream s;
            s << "Expected a paramter list, got: " << expr->toString();
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);

            return true;
        }

        bool describeFormals(unsigned & len) const
        {
            if ( formals->isSymbol() ) {
                len = 0;
                return true;
            }

            len = formals->length();
            if ( ! formals->isList() ) {
                --len;
                return true;
            }
            return false;
        }

        void
        wrongNumberOfParameters(unsigned formalsLen, unsigned actualsLen) const
        {
            stringstream s;
            s << "Expected " << formalsLen << " parameters; "
              << "got " << actualsLen;
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
        }

        bool checkArgLength(ScamExpr * expr) const
        {
            unsigned exp { 0 };
            bool optFinal = describeFormals(exp);
            unsigned act = expr->length();

            if ( (act < exp) || ((! optFinal) && (act > exp)) ) {
                wrongNumberOfParameters(exp, act);
                return false;
            }

            return true;
        }

        void finalize(ScamExpr * actuals)  const
        {
            Binder binder(capture);
            Env extended = binder.bind(formals, actuals);

            using WT = EvalWorker;
            ScamExpr * f = forms;

            if ( macrolike ) {
                ContHandle cont2 = make_shared<MacroEvalCont>(cont, capture);
                workQueueHelper<WT>(f, extended, cont2);
            }
            else {
                workQueueHelper<WT>(f, extended, cont);
            }
        }
    };

    class ClosureWorker : public Worker
    {
    public:
        ClosureWorker(ScamExpr *formals,
                      ScamExpr * forms,
                      Env capture,
                      ContHandle cont,
                      ScamExpr * args,
                      Env argEnv,
                      bool macrolike)
            : Worker("proc")
            , formals(formals)
            , forms(forms)
            , capture(capture)
            , cont(cont)
            , args(args)
            , argEnv(argEnv)
            , macrolike(macrolike)
        {
        }

        void run() override
        {
            Worker::run();

            ContHandle newCont
                = make_shared<ClosureBindCont>(formals,
                                               forms,
                                               capture,
                                               cont,
                                               macrolike);
            if ( macrolike ) {
                newCont->run(args);
            }
            else {
                args->mapEval(newCont, argEnv);
            }
        }

    private:
        ScamExpr * formals;
        ScamExpr * forms;
        Env capture;
        ContHandle cont;
        ScamExpr * args;
        Env argEnv;
        bool macrolike;
    };

    void do_apply(ScamExpr * formals,
                  ScamExpr * forms,
                  Env capture,
                  ContHandle cont,
                  ScamExpr * args,
                  Env argEnv,
                  bool macrolike)
    {
        workQueueHelper<ClosureWorker>(formals,
                                       forms,
                                       capture,
                                       cont,
                                       args,
                                       argEnv,
                                       macrolike);
    }
}
