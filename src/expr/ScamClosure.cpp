
#include "expr/ScamClosure.hpp"

#include "Binder.hpp"
#include "Continuation.hpp"
#include "EvalWorker.hpp"
#include "Extractor.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"

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
                  Env argEnv);
}

ScamClosure::ScamClosure(ScamExpr *formals, ScamExpr * forms, Env env)
    : formals(formals->clone())
    , forms(forms->clone())
    , env(env)
{
}

string ScamClosure::toString() const
{
    stringstream s;
    s << "(proc " << formals->toString() << " " << forms->toString() << ")";
    return s.str();
}

bool ScamClosure::hasApply() const
{
    return true;
}

void ScamClosure::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(formals.get(), forms.get(), this->env, cont, args, env);
}

bool ScamClosure::isProcedure() const
{
    return true;
}

namespace
{
    class ClosureBindCont : public Continuation
    {
    public:
        ClosureBindCont(ScamExpr * formals,
                        ScamExpr * forms,
                        Env capture,
                        ContHandle cont)
            : Continuation("proc - bind")
            , formals(formals->clone())
            , forms(forms->clone())
            , capture(capture)
            , cont(cont)
        {
        }

        void run(ScamExpr * expr) override
        {
            if ( expr->error() ) {
                cont->run(expr);
            }
            else if ( ! expr->isCons() && ! expr->isNil() ) {
                malformedActuals(expr);
            }
            else if ( checkArgLength(expr) ) {
                finalize(expr);
            }
        }

    private:
        ExprHandle formals;
        ExprHandle forms;
        Env        capture;
        ContHandle cont;

        void malformedActuals(ScamExpr * expr) const
        {
            stringstream s;
            s << "Expected a paramter list, got: " << expr->toString();
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
        }

        bool describeFormals(unsigned & len) const
        {
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
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
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
            Env extended = binder.bind(formals.get(), actuals);

            using WT = EvalWorker;
            ScamExpr * f = forms.get();
            workQueueHelper<WT>(f, extended, cont);
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
                      Env argEnv)
            : Worker("proc")
            , formals(formals->clone())
            , forms(forms->clone())
            , capture(capture)
            , cont(cont)
            , args(args->clone())
            , argEnv(argEnv)
        {
        }

        void run() override
        {
            ContHandle newCont
                = make_shared<ClosureBindCont>(formals.get(),
                                               forms.get(),
                                               capture,
                                               cont);
            args->mapEval(newCont, argEnv);
        }

    private:
        ExprHandle formals;
        ExprHandle forms;
        Env capture;
        ContHandle cont;
        ExprHandle args;
        Env argEnv;
    };

    void do_apply(ScamExpr * formals,
                  ScamExpr * forms,
                  Env capture,
                  ContHandle cont,
                  ScamExpr * args,
                  Env argEnv)
    {
        workQueueHelper<ClosureWorker>(formals,
                                       forms,
                                       capture,
                                       cont,
                                       args,
                                       argEnv);
    }
}
