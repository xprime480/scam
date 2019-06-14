#include "form/OrWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/ValueFactory.hpp"
#include "form/OrCont.hpp"
#include "input/ListParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

OrWorker::OrWorker(Continuation * cont,
                   Env * env,
                   ListParser * parser,
                   ScamEngine * engine,
                   size_t n)
    : Worker("Or", engine)
    , parser(parser)
    , cont(cont)
    , env(env)
    , n(n)
{
}

OrWorker * OrWorker::makeInstance(Continuation * cont,
                                  Env * env,
                                  ListParser * parser,
                                  ScamEngine * engine,
                                  size_t n)
{
    return new OrWorker(cont, env, parser, engine, n);
}

void OrWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void OrWorker::run()
{
    Worker::run();

    size_t const len = parser->size();
    if ( 0 == len ) {
        ScamValue rv = makeBoolean(false);
        cont->handleValue(rv);
    }
    else if ( n == (len - 1) ) {
        eval(parser->get(len-1), cont, env, engine);
    }
    else {
        ScamValue test = parser->get(n);
        Continuation * newCont =
            standardMemoryManager.make<OrCont>(parser, cont, env, engine, n+1);
        eval(test, newCont, env, engine);
    }
}
