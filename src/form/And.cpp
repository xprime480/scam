#include "form/And.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/AndWorker.hpp"
#include "input/ListParser.hpp"

using namespace scam;
using namespace std;

And::And()
    : SpecialForm("and")
{
}

And * And::makeInstance()
{
    static And instance;
    return &instance;
}

void And::apply(ExprHandle args, Continuation * cont, Env * env)
{
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("and expects (form...); got: ",
                                         args->toString());
        cont->run(err);
        return;
    }

    unsigned pos { 0 };
    workQueueHelper<AndWorker>(cont, env, parser, pos);
}
