#include "form/LetWorker.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/LetEvalWorker.hpp"

using namespace scam;
using namespace std;

LetWorker::LetWorker(ExprHandle args,
                     Continuation * cont,
                     Env * env,
                     bool rebind)
    : LetBaseWorker("Let", args, cont, env)
    , rebind(rebind)
{
}

LetWorker * LetWorker::makeInstance(ExprHandle args,
                                    Continuation * cont,
                                    Env * env,
                                    bool rebind)
{
    return new LetWorker(args, cont, env, rebind);
}

void LetWorker::do_next(ExprHandle formals,
                        ExprHandle values,
                        ExprHandle forms)
{
    ExprHandle evaled = ExpressionFactory::makeNil();
    ExprHandle e = evaled;
    workQueueHelper<LetEvalWorker>(formals,
                                   e,
                                   values,
                                   forms,
                                   cont,
                                   env,
                                   rebind);
}
