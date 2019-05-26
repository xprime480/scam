#include "form/UndefineWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamData.hpp"
#include "form/UndefineCont.hpp"
#include "input/UndefineParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

UndefineWorker::UndefineWorker(UndefineParser * parser,
                               Continuation * cont,
                               Env * env)
    : Worker("Undefine")
    , parser(parser)
    , cont(cont)
    , env(env)
{
}

UndefineWorker * UndefineWorker::makeInstance(UndefineParser * parser,
                                              Continuation * cont,
                                              Env * env)
{
    return new UndefineWorker(parser, cont, env);
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

    ScamEnvKeyType sym = parser->getSymbol();
    Continuation * c = standardMemoryManager.make<UndefineCont>(sym, cont, env);
    ScamValue expr = ExpressionFactory::makeNil();

    c->run(expr);
}
