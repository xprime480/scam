#include "form/Assign.hpp"

#include "WorkQueue.hpp"
#include "form/AssignWorker.hpp"

using namespace scam;
using namespace std;

Assign::Assign(ScamEngine * engine)
    : EnvHelper("assign!", engine)
{
}

Assign * Assign::makeInstance(ScamEngine * engine)
{
    return new Assign(engine);
}

void Assign::apply(ExprHandle args, Continuation * cont, Env * env)
{
    if ( checkArgs(args, cont, true) ) {
        workQueueHelper<AssignWorker>(args, cont, env, engine);
    }
}
