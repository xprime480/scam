#include "form/LetRec.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/LetWorker.hpp"
#include "input/LetParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

LetRec::LetRec()
    : SpecialForm("letrec")
{
}

LetRec * LetRec::makeInstance()
{
    static LetRec instance;
    return &instance;
}

void LetRec::apply(ExprHandle args, Continuation * cont, Env * env)
{
    LetParser * parser = standardMemoryManager.make<LetParser>();
    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("letrec expects ",
                                         "(((sym form)*) form*)",
                                         "; got: ",
                                         args->toString());
        cont->run(err);
    }
    else {
        workQueueHelper<LetWorker>(parser, cont, env, true);
    }
}
