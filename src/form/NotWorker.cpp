#include "form/NotWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "form/NotCont.hpp"
#include "input/SingletonParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

NotWorker::NotWorker(Continuation * cont,
                     Env * env,
                     ScamEngine * engine,
                     SingletonParser * parser)
    : Worker("Not", engine)
    , parser(parser)
    , cont(cont)
    , env(env)
{
}

NotWorker * NotWorker::makeInstance(Continuation * cont,
                                    Env * env,
                                    ScamEngine * engine,
                                    SingletonParser * parser)
{
    return new NotWorker(cont, env, engine, parser);
}

void NotWorker::mark()
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

    Continuation * newCont = standardMemoryManager.make<NotCont>(cont, engine);
    ScamValue form = parser->get();
    eval(form, newCont, env, engine);
}

