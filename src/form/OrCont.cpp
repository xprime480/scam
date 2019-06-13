#include "form/OrCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "form/OrWorker.hpp"
#include "input/ListParser.hpp"

using namespace scam;
using namespace std;

OrCont::OrCont(ListParser * parser,
               Continuation * cont,
               Env * env,
               ScamEngine * engine,
               size_t n)
    : Continuation("Or", engine)
    , parser(parser)
    , cont(cont)
    , env(env)
    , n(n)
{
}

OrCont * OrCont::makeInstance(ListParser * parser,
                              Continuation * cont,
                              Env * env,
                              ScamEngine * engine,
                              size_t n)
{
    return new OrCont(parser, cont, env, engine, n);
}

void OrCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void OrCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( error(expr) ) {
        cont->run(expr);
    }
    else if ( truth(expr) ) {
        cont->run(expr);
    }
    else {
        workQueueHelper<OrWorker>(cont, env, parser, engine, n);
    }
}
