#include "form/LetWorker.hpp"

#include "WorkQueue.hpp"
#include "expr/ValueFactory.hpp"
#include "form/LetEvalWorker.hpp"

using namespace scam;
using namespace std;

LetWorker::LetWorker(LetDef & def,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine,
                     bool rebind)
    : LetBaseWorker("Let", def, cont, env, engine)
    , rebind(rebind)
{
}

LetWorker * LetWorker::makeInstance(LetDef & def,
                                    Continuation * cont,
                                    Env * env,
                                    ScamEngine * engine,
                                    bool rebind)
{
    return new LetWorker(def, cont, env, engine, rebind);
}

void LetWorker::do_next(ScamValue formals,
                        ScamValue values,
                        ScamValue forms)
{
    ScamValue evaled = makeNull();
    workQueueHelper<LetEvalWorker>(formals,
                                   evaled,
                                   values,
                                   forms,
                                   cont,
                                   env,
                                   engine,
                                   rebind);
}
