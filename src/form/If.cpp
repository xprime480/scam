#include "form/If.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/IfWorker.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

If::If()
    : SpecialForm("if")
{
}

If * If::makeInstance()
{
    static If instance;
    return &instance;
}

void If::apply(ExprHandle args, Continuation * cont, Env * env)
{
    CountedListParser * parser = getCountedListOfAnythingParser(2, 3);

    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("if expected",
                                         " (test then-expr [else-expr])",
                                         "; got: ",
                                         args->toString());
        cont->run(err);
    }
    else {
        workQueueHelper<IfWorker>(cont, env, parser);
    }
}
