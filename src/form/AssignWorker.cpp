#include "form/AssignWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamExpr.hpp"
#include "form/AssignCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AssignWorker::AssignWorker(ScamExpr * args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
    : EnvHelperWorker(args, cont, env, "Assign")
    , engine(engine)
{
}

AssignWorker * AssignWorker::makeInstance(ScamExpr * args,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine)
{
    return new AssignWorker(args, cont, env, engine);
}

Continuation * AssignWorker::getCont(ScamExpr * sym) const
{
    return standardMemoryManager.make<AssignCont>(sym,
                                                  cont,
                                                  env,
                                                  engine);
}

