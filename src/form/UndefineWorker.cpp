#include "form/UndefineWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
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

void UndefineWorker::mark()
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

    if ( env->check(sym, false) ) {
        Continuation * c =
            standardMemoryManager.make<UndefineCont>(sym, cont, env, engine);
        c->handleValue(makeNull());
    }
    else {
        static ScamValue err =
            makeErrorExtended("Symbol '"
                              , writeValue(sym),
                              "' does not exist in current scope");
        engine->handleError(err);
    }
}
