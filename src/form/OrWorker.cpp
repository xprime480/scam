#include "form/OrWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/OrCont.hpp"
#include "input/ListParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

OrWorker::OrWorker(Continuation * cont,
                   Env * env,
                   ListParser * parser,
                   size_t n)
    : Worker("Or")
    , parser(parser)
    , cont(cont)
    , env(env)
    , n(n)
{
}

OrWorker * OrWorker::makeInstance(Continuation * cont,
                                  Env * env,
                                  ListParser * parser,
                                  size_t n)
{
    return new OrWorker(cont, env, parser, n);
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
        ScamValue rv = ExpressionFactory::makeBoolean(false);
        cont->run(rv);
    }
    else if ( n == (len - 1) ) {
        parser->get(len-1)->eval(cont, env);
    }
    else {
        ScamValue test = parser->get(n);
        Continuation * newCont =
            standardMemoryManager.make<OrCont>(parser, cont, env, n+1);
        test->eval(newCont, env);
    }
}
