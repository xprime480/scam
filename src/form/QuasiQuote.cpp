#include "form/QuasiQuote.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/QuasiQuoteWorker.hpp"

using namespace scam;
using namespace std;

ExprHandle const QuasiQuote::spliceTag =
    ExpressionFactory::makeSymbol("**splicing**", false);

QuasiQuote::QuasiQuote()
    : SpecialForm("quasiquote")
{
}

QuasiQuote * QuasiQuote::makeInstance()
{
    static QuasiQuote instance;
    return &instance;
}

void QuasiQuote::apply(ExprHandle args, Continuation * cont, Env * env)
{
    qq_apply(args, cont, env, true);
}

void QuasiQuote::qq_apply(ExprHandle args,
                          Continuation * cont,
                          Env * env,
                          bool top)
{
    if ( ! args->isList() ) {
        ExprHandle err =
            ExpressionFactory::makeError("quasiquote expecting list of args",
                                         ", got ",
                                         args->toString());
        cont->run(err);
    }
    else if ( top ) {
        if ( 1 != args->length() ) {
            ExprHandle err =
                ExpressionFactory::makeError("quasiquote expecting one form",
                                             ", got ",
                                             args->toString());
            cont->run(err);
        }
        else {
            ExprHandle form = args->nthcar(0);
            workQueueHelper<QuasiQuoteWorker>(form, cont, env);
        }
    }
    else {
        ExprHandle form = args;
        workQueueHelper<QuasiQuoteWorker>(form, cont, env);
    }
}
