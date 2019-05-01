#include "form/DefineWorker.hpp"

#include "Env.hpp"
#include "form/DefineCont.hpp"
#include "input/AssignParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

DefineWorker::DefineWorker(AssignParser * parser,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
    : Worker("Define")
    , parser(parser)
    , cont(cont)
    , env(env)
    , engine(engine)
{
}

DefineWorker * DefineWorker::makeInstance(AssignParser * parser,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine)
{
    return new DefineWorker(parser, cont, env, engine);
}

void DefineWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void DefineWorker::run()
{
    Worker::run();

    ScamEnvKeyType sym = parser->getSymbol();
    Continuation * c =
        standardMemoryManager.make<DefineCont>(sym, cont, env, engine);

    ExprHandle expr = parser->getForm();
    expr->eval(c, env);
}





