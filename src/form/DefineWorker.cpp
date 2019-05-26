#include "form/DefineWorker.hpp"

#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "form/DefineCont.hpp"
#include "input/SymbolPlusParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

DefineWorker::DefineWorker(DefineParser * parser,
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

DefineWorker * DefineWorker::makeInstance(DefineParser * parser,
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

    ScamValue sym = parser->getSymbol();
    Continuation * c =
        standardMemoryManager.make<DefineCont>(sym, cont, env, engine);

    ScamValue expr = parser->getForm();
    eval(expr, c, env);
}
