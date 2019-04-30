#include "form/Define.hpp"

#include "WorkQueue.hpp"
#include "form/DefineWorker.hpp"

using namespace scam;
using namespace std;

Define::Define(ScamEngine * engine)
    : EnvHelper("define", engine)
{
}

Define * Define::makeInstance(ScamEngine * engine)
{
    return new Define(engine);
}

void Define::apply(ExprHandle args, Continuation * cont, Env * env)
{
    if ( checkArgs(args, cont, true) ) {
        workQueueHelper<DefineWorker>(args, cont, env, engine);
    }
}
