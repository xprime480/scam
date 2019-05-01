#include "form/Undefine.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/UndefineWorker.hpp"
#include "input/UndefineParser.hpp"

using namespace scam;
using namespace std;

Undefine::Undefine()
    : SpecialForm("assign!", true)
{
}

Undefine * Undefine::makeInstance()
{
    return new Undefine();
}

void Undefine::apply(ExprHandle args, Continuation * cont, Env * env)
{
    UndefineParser * parser = standardMemoryManager.make<UndefineParser>();

    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("undefine expects (sym); got: ",
                                         args->toString());
        cont->run(err);
    }
    else {
        workQueueHelper<UndefineWorker>(parser, cont, env);
    }
}
