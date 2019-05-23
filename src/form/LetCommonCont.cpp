#include "form/LetCommonCont.hpp"

#include "Continuation.hpp"
#include "EvalWorker.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

LetCommonCont::LetCommonCont(char const * name,
                             ExprHandle forms,
                             Continuation * cont)
    : Continuation(name)
    , forms(forms)
    , cont(cont)
{
}

void LetCommonCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        forms->mark();
        cont->mark();
    }
}

void LetCommonCont::run(ExprHandle expr)
{
    Continuation::run(expr);

    if ( TypePredicates::error(expr) ) {
        cont->run(expr);
    }
    else {
        do_let(expr);
    }
}

void LetCommonCont::final_eval(Env * env)
{
    workQueueHelper<EvalWorker>(forms, env, cont);
}

