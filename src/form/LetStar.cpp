#include "form/LetStar.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"
#include "form/LetStarWorker.hpp"
#include "input/LetParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "let*";

LetStar::LetStar(ScamEngine * engine)
    : SpecialForm(myName, applyLetStar, engine, true)
{
}

LetStar * LetStar::makeInstance(ScamEngine * engine)
{
    return new LetStar(engine);
}

ScamValue LetStar::safeCons(ScamValue expr)
{
    if ( isCons(expr) ) {
        return expr;
    }
    return ExpressionFactory::makeList(expr);
}

void scam::applyLetStar(ScamValue args,
                        Continuation * cont,
                        Env * env,
                        ScamEngine * engine)
{
    LetParser * parser = standardMemoryManager.make<LetParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(((sym form)*) form*)", args, cont);
    }
    else {
        workQueueHelper<LetStarWorker>(parser, cont, env, engine);
    }
}
