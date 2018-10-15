
#include "expr/ScamClosure.hpp"

#include "Continuation.hpp"
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
    class ClosureFinalCont : public Continuation
    {
    public:
        ClosureFinalCont(ContHandle cont)
            : Continuation("proc")
            , cont(cont)
        {
        }

        void run(ScamExpr * expr) override
        {
            if ( expr->isList() ) {
                unsigned len = expr->length();
                ExprHandle last = expr->nth(len - 1);
                cont->run(last.get());
            }
            else {
                cont->run(expr);
            }
        }

    private:
        ContHandle cont;
    };

    class ClosureFinalWorker : public Worker
    {
    public:
        ClosureFinalWorker(ScamExpr * forms, Env extended, ContHandle cont)
            : Worker("proc - eval")
            , forms(forms->clone())
            , extended(extended)
            , cont(cont)
        {
        }

        void run() override
        {
            ContHandle newCont = make_shared<ClosureFinalCont>(cont);
            forms->mapEval(newCont, extended);
        }

    private:
        ExprHandle forms;
        Env extended;
        ContHandle cont;
    };

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
            else if ( ! expr->isList() ) {
                stringstream s;
                ExprHandle err = ExpressionFactory::makeError(s.str());
                cont->run(err.get());
            }
            else {
                unsigned formalsLen = formals->length();
                unsigned actualsLen = expr->length();
                if ( formalsLen != actualsLen ) {
                    stringstream s;
                    s << "Expected " << formalsLen << " parameters; "
                      << "got " << actualsLen;
                    ExprHandle err = ExpressionFactory::makeError(s.str());
                    cont->run(err.get());
                }
                else {
                    Env extended = capture.extend();
                    for ( unsigned idx = 0 ; idx < formalsLen ; ++idx ) {
                        ExprHandle sym = formals->nth(idx);
                        ExprHandle val = expr->nth(idx);
                        extended.put(sym.get(), val.get());
                    }

                    using WT = ClosureFinalWorker;
                    ScamExpr * f = forms.get();
                    workQueueHelper<WT>(f, extended, cont);
                }
            }
        }

    private:
        ExprHandle formals;
        ExprHandle forms;
        Env capture;
        ContHandle cont;
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
