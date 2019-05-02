#include "form/Let.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/LetWorker.hpp"
#include "input/LetParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

Let::Let()
    : SpecialForm("let")
{
}

Let * Let::makeInstance()
{
    static Let instance;
    return &instance;
}

void Let::apply(ExprHandle args, Continuation * cont, Env * env)
{
    LetParser * parser = standardMemoryManager.make<LetParser>();
    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("let expects (((sym form)*) form*)",
                                         "; got: ",
                                         args->toString());
        cont->run(err);
    }
    else {
        workQueueHelper<LetWorker>(parser, cont, env, false);
    }
}
