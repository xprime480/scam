#include "form/Define.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/DefineWorker.hpp"
#include "input/AssignParser.hpp"

using namespace scam;
using namespace std;

Define::Define(ScamEngine * engine)
    : SpecialForm("define", true)
    , engine(engine)
{
}

Define * Define::makeInstance(ScamEngine * engine)
{
    return new Define(engine);
}

void Define::apply(ExprHandle args, Continuation * cont, Env * env)
{
    /*
     * For now, define looks like assign.
     *
     * This is not in accord with the language definition, so it
     * should change in the future.
     */
    AssignParser * parser = standardMemoryManager.make<AssignParser>();

    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("define expects (sym expr)",
                                         "; got: ",
                                         args->toString());
        cont->run(err);
    }
    else {
        workQueueHelper<DefineWorker>(parser, cont, env, engine);
    }
}
