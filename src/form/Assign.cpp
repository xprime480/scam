#include "form/Assign.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/AssignWorker.hpp"
#include "input/SymbolPlusParser.hpp"

using namespace scam;
using namespace std;

Assign::Assign(ScamEngine * engine)
    : SpecialForm("assign!", true)
    , engine(engine)
{
}

Assign * Assign::makeInstance(ScamEngine * engine)
{
    return new Assign(engine);
}

void Assign::apply(ExprHandle args, Continuation * cont, Env * env)
{
    AssignParser * parser = standardMemoryManager.make<AssignParser>();

    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("assign! expects (sym expr)",
                                         "; got: ",
                                         args->toString());
        cont->run(err);
    }
    else {
        workQueueHelper<AssignWorker>(parser, cont, env, engine);
    }
}
