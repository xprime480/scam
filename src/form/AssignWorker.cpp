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
    : Worker("Assign", engine)
    , parser(parser)
    , cont(cont)
    , env(env)
{
}

AssignWorker * AssignWorker::makeInstance(AssignParser * parser,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine)
{
    return new AssignWorker(parser, cont, env, engine);
}

void AssignWorker::mark()
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

    ScamValue sym = parser->getSymbol();
    Continuation * c = standardMemoryManager.make<AssignCont>(sym,
                                                              cont,
                                                              env,
                                                              engine);

    ScamValue expr = parser->getForm();
    eval(expr, c, env, engine);
}
