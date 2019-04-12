#include "form/UndefineWorker.hpp"

#include "form/UndefineCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

UndefineWorker::UndefineWorker(ScamExpr * args,
                               Continuation * cont,
                               Env * env,
                               ScamEngine * engine)
    : EnvHelperWorker(args, cont, env, "Undefine")
{
}

UndefineWorker * UndefineWorker::makeInstance(ScamExpr * args,
                                              Continuation * cont,
                                              Env * env,
                                              ScamEngine * engine)
{
    return new UndefineWorker(args, cont, env, engine);
}

Continuation * UndefineWorker::getCont(ScamExpr * sym) const
{
    return standardMemoryManager.make<UndefineCont>(sym, cont, env);
}

