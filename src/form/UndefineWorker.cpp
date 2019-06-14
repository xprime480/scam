#include "form/UndefineWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/ValueFactory.hpp"
#include "form/UndefineCont.hpp"
#include "input/UndefineParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

UndefineWorker::UndefineWorker(UndefineParser * parser,
                               Continuation * cont,
                               Env * env,
                               ScamEngine * engine)
    : Worker("Undefine", engine)
    , parser(parser)
    , cont(cont)
    , env(env)
{
}

UndefineWorker * UndefineWorker::makeInstance(UndefineParser * parser,
                                              Continuation * cont,
                                              Env * env,
                                              ScamEngine * engine)
{
    return new UndefineWorker(parser, cont, env, engine);
}

void UndefineWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void UndefineWorker::run()
{
    Worker::run();

    ScamValue sym = parser->getSymbol();
    Continuation * c =
        standardMemoryManager.make<UndefineCont>(sym, cont, env, engine);
    ScamValue expr = makeNull();

    c->handleValue(expr);
}
