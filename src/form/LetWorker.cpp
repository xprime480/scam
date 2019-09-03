#include "form/LetWorker.hpp"

#include "WorkQueue.hpp"
#include "expr/ValueFactory.hpp"
#include "form/LetEvalWorker.hpp"

using namespace scam;
using namespace std;

LetWorker::LetWorker(LetDef & def, Continuation * cont, Env * env, bool rebind)
    : LetBaseWorker("Let", def, cont, env)
    , rebind(rebind)
{
}

LetWorker *
LetWorker::makeInstance(LetDef & def,
                        Continuation * cont,
                        Env * env,
                        bool rebind)
{
    return new LetWorker(def, cont, env, rebind);
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
                                   rebind);
}
