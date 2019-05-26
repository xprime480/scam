#include "form/AssignWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "form/AssignCont.hpp"
#include "input/SymbolPlusParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AssignWorker::AssignWorker(AssignParser * parser,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
    : Worker("Assign")
    , parser(parser)
    , cont(cont)
    , env(env)
    , engine(engine)
{
}

AssignWorker * AssignWorker::makeInstance(AssignParser * parser,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine)
{
    return new AssignWorker(parser, cont, env, engine);
}

void AssignWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void AssignWorker::run()
{
    Worker::run();

    ScamEnvKeyType sym = parser->getSymbol();
    Continuation * c = standardMemoryManager.make<AssignCont>(sym,
                                                              cont,
                                                              env,
                                                              engine);

    ScamValue expr = parser->getForm();
    eval(expr, c, env);
}
