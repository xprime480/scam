#include "form/QuasiQuote.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/QuasiQuoteWorker.hpp"
#include "input/SingletonParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

ExprHandle const QuasiQuote::spliceTag =
    ExpressionFactory::makeSymbol("**splicing**", false);

static const char * myName = "quasiquote";

QuasiQuote::QuasiQuote()
    : SpecialForm(myName)
{
}

QuasiQuote * QuasiQuote::makeInstance()
{
    static QuasiQuote instance;
    return &instance;
}

void QuasiQuote::apply(ExprHandle args, Continuation * cont, Env * env)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont);
    }
    else {
        workQueueHelper<QuasiQuoteWorker>(parser->get(), cont, env);
    }
}
