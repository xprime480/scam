#include "form/Undefine.hpp"

#include "WorkQueue.hpp"
#include "form/UndefineWorker.hpp"

using namespace scam;
using namespace std;

Undefine::Undefine(ScamEngine * engine)
  : EnvHelper("undefine", engine)
{
}

Undefine * Undefine::makeInstance(ScamEngine * engine)
{
    return new Undefine(engine);
}

void Undefine::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    if ( checkArgs(args, cont, false) ) {
        workQueueHelper<UndefineWorker>(args, cont, env, engine);
    }
}
