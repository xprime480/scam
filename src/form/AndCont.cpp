#include "form/AndCont.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "form/AndWorker.hpp"
#include "input/ListParser.hpp"

using namespace scam;
using namespace std;

AndCont::AndCont(ListParser * parser,
                 Continuation * cont,
                 Env * env,
                 ScamEngine * engine,
                 size_t n)
    : Continuation("And", engine)
    , parser(parser)
    , cont(cont)
    , env(env)
    , n(n)
{
}

AndCont * AndCont::makeInstance(ListParser * parser,
                                Continuation * cont,
                                Env * env,
                                ScamEngine * engine,
                                size_t n)
{
    return new AndCont(parser, cont, env, engine, n);
}

void AndCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void AndCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        engine->handleError(value);
    }
    else if ( ! truth(value) ) {
        cont->handleValue(value);
    }
    else {
        workQueueHelper<AndWorker>(cont, env, parser, engine, n);
    }
}
