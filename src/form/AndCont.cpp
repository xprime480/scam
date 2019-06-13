#include "form/AndCont.hpp"

#include "Env.hpp"
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

void AndCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void AndCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( error(expr) ) {
        cont->run(expr);
    }
    else if ( ! truth(expr) ) {
        cont->run(expr);
    }
    else {
        workQueueHelper<AndWorker>(cont, env, parser, engine, n);
    }
}
