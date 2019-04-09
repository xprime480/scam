
#include "form/CallCC.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void do_apply(ScamExpr * args, ContHandle cont, Env env);
}

CallCC::CallCC()
    : SpecialForm("call/cc")
{
}

CallCC * CallCC::makeInstance()
{
    static CallCC instance;
    return &instance;
}

void CallCC::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(args, cont, env);
}

namespace
{
    class CallCont : public Continuation
    {
    public:
        CallCont(ContHandle cont, Env env)
            : Continuation("CallCont")
            , cont(cont)
            , env(env)
        {
        }

        void run(ScamExpr * expr) override
        {
            if ( expr->error() ) {
                cont->run(expr);
                return;
            }

            if ( ! expr->hasApply() ) {
                stringstream s;
                s << "call/cc: form " << expr->toString()
                  << "cannot be applied";
                ScamExpr * err = ExpressionFactory::makeError(s.str());
                cont->run(err);
                return;
            }

            ScamExpr * contExpr = ExpressionFactory::makeContinuation(cont);
            ScamExpr * args = ExpressionFactory::makeList(contExpr);
            expr->apply(args, cont, env);
        }

    private:
        ContHandle cont;
        Env        env;
    };

    void do_apply(ScamExpr * args, ContHandle cont, Env env)
    {
        ScamExpr * body = args->nthcar(0);
        ContHandle newCont = make_shared<CallCont>(cont, env);
        body->eval(newCont, env);
    }
}
