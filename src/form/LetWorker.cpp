#include "form/LetWorker.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/LetEvalWorker.hpp"

using namespace scam;
using namespace std;

LetWorker::LetWorker(LetParser * parser,
                     Continuation * cont,
                     Env * env,
                     bool rebind)
    : LetBaseWorker("Let", parser, cont, env)
    , rebind(rebind)
{
}

LetWorker * LetWorker::makeInstance(LetParser * parser,
                                    Continuation * cont,
                                    Env * env,
                                    bool rebind)
{
    return new LetWorker(parser, cont, env, rebind);
}

void LetWorker::do_next(ScamValue formals,
                        ScamValue values,
                        ScamValue forms)
{
    ScamValue evaled = ExpressionFactory::makeNil();
    ScamValue e = evaled;
    workQueueHelper<LetEvalWorker>(formals,
                                   e,
                                   values,
                                   forms,
                                   cont,
                                   env,
                                   rebind);
}
