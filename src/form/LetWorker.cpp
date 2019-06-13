#include "form/LetWorker.hpp"

#include "WorkQueue.hpp"
#include "expr/ValueFactory.hpp"
#include "form/LetEvalWorker.hpp"

using namespace scam;
using namespace std;

LetWorker::LetWorker(LetParser * parser,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine,
                     bool rebind)
    : LetBaseWorker("Let", parser, cont, env, engine)
    , rebind(rebind)
{
}

LetWorker * LetWorker::makeInstance(LetParser * parser,
                                    Continuation * cont,
                                    Env * env,
                                    ScamEngine * engine,
                                    bool rebind)
{
    return new LetWorker(parser, cont, env, engine, rebind);
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
