#include "form/IfWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "form/IFCont.hpp"
#include "input/CountedListParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

IfWorker::IfWorker(Continuation * cont, Env * env, CountedListParser * parser)
    : Worker("If")
    , parser(parser)
    , cont(cont)
    , env(env)
{
}

IfWorker * IfWorker::makeInstance(Continuation * cont,
                                  Env * env,
                                  CountedListParser * parser)
{
    return new IfWorker(cont, env, parser);
}

void IfWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        cont->mark();
        env->mark();
        parser->mark();
    }
}

void IfWorker::run()
{
    Worker::run();

    Continuation * newCont =
        standardMemoryManager.make<IfCont>(parser, cont, env);
    ScamValue test = parser->get(0u);

    eval(test, newCont, env);
}
