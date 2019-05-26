#include "form/NotWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/NotCont.hpp"
#include "input/SingletonParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

NotWorker::NotWorker(Continuation * cont, Env * env, SingletonParser * parser)
    : Worker("Not")
    , parser(parser)
    , cont(cont)
    , env(env)
{
}

NotWorker * NotWorker::makeInstance(Continuation * cont,
                                    Env * env,
                                    SingletonParser * parser)
{
    return new NotWorker(cont, env, parser);
}

void NotWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void NotWorker::run()
{
    Worker::run();

    Continuation * newCont = standardMemoryManager.make<NotCont>(cont);
    ScamValue form = parser->get();
    eval(form, newCont, env);
}

