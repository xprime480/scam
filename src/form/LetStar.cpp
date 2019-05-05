#include "form/LetStar.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/LetStarWorker.hpp"
#include "input/LetParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "let*";

LetStar::LetStar(ScamEngine * engine)
    : SpecialForm(myName, true)
    , engine(engine)
{
}

LetStar * LetStar::makeInstance(ScamEngine * engine)
{
    return new LetStar(engine);
}

ExprHandle LetStar::safeCons(ExprHandle expr)
{
    if ( expr->isCons() ) {
        return expr;
    }
    return ExpressionFactory::makeList(expr);
}

void LetStar::apply(ExprHandle args, Continuation * cont, Env * env)
{
    LetParser * parser = standardMemoryManager.make<LetParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(((sym form)*) form*)", args, cont);
    }
    else {
        workQueueHelper<LetStarWorker>(parser, cont, env, engine);
    }
}
