#include "form/AndWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AndCont.hpp"
#include "input/ListParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AndWorker::AndWorker(Continuation * cont,
                     Env * env,
                     ListParser * parser,
                     ScamEngine * engine,
                     size_t n)
    : Worker("And", engine)
    , cont(cont)
    , env(env)
    , parser(parser)
    , n(n)
{
}

AndWorker * AndWorker::makeInstance(Continuation * cont,
                                    Env * env,
                                    ListParser * parser,
                                    ScamEngine * engine,
                                    size_t n)
{
    return new AndWorker(cont, env, parser, engine, n);
}

void AndWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        cont->mark();
        env->mark();
        parser->mark();
    }
}

void AndWorker::run()
{
    Worker::run();

    size_t const len = parser->size();
    if ( 0 == len ) {
        cont->handleValue(makeBoolean(true));
    }
    else if ( n == (len - 1) ) {
        eval(parser->get(len-1), cont, env, engine);
    }
    else {
        ScamValue test = parser->get(n);
        Continuation * newCont =
            standardMemoryManager.make<AndCont>(parser, cont, env, engine, n+1);
        eval(test, newCont, env, engine);
    }
}
