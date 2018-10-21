
#include "EvalWorker.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

namespace
{
    class EvalCont : public Continuation
    {
    public:
        EvalCont(ContHandle cont)
            : Continuation("eval")
            , cont(cont)
        {
        }

        void run(ScamExpr * expr) override
        {
            if ( expr->isList() && ! expr->isNil() ) {
                unsigned len = expr->length();
                ExprHandle last = expr->nthcar(len - 1);
                cont->run(last.get());
            }
            else {
                cont->run(expr);
            }
        }

    private:
        ContHandle cont;
    };
}


EvalWorker::EvalWorker(ScamExpr * forms, Env extended, ContHandle cont)
    : Worker("eval")
    , forms(forms->clone())
    , extended(extended)
    , cont(cont)
{
}

void EvalWorker::run()
{
    ContHandle newCont = make_shared<EvalCont>(cont);
    forms->mapEval(newCont, extended);
}
