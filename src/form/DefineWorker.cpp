#include "form/DefineWorker.hpp"

#include "form/DefineCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

DefineWorker::DefineWorker(ExprHandle args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
    : EnvHelperWorker(args, cont, env, "Define")
    , engine(engine)
{
}

DefineWorker * DefineWorker::makeInstance(ExprHandle args,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine)
{
    return new DefineWorker(args, cont, env, engine);
}

Continuation * DefineWorker::getCont(ScamEnvKeyType sym) const
{
    return standardMemoryManager.make<DefineCont>(sym,
                                                  cont,
                                                  env,
                                                  engine);
}

