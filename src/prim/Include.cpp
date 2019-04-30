#include "prim/Include.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "prim/IncludeWorker.hpp"

using namespace scam;
using namespace std;

Include::Include(ScamEngine * engine)
    : Primitive("include")
    , engine(engine)
{
}

Include * Include::makeInstance(ScamEngine * engine)
{
    return new Include(engine);
}

void Include::applyArgs(ExprHandle args, Continuation * cont)
{
    if ( args->length() < 1 ) {
        ExprHandle err =
            ExpressionFactory::makeError("include expects at least 1 filename",
                                         "; got 0");
        cont->run(err);
    }
    else {
        workQueueHelper<IncludeWorker>(args, cont, engine);
    }
}

