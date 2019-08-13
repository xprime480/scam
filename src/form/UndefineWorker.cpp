#include "form/UndefineWorker.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "form/UndefineCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

UndefineWorker::UndefineWorker(ScamValue value,
                               Continuation * cont,
                               Env * env,
                               ScamEngine * engine)
    : Worker("Undefine", engine)
    , value(value)
    , cont(cont)
    , env(env)
{
}

UndefineWorker * UndefineWorker::makeInstance(ScamValue value,
                                              Continuation * cont,
                                              Env * env,
                                              ScamEngine * engine)
{
    return new UndefineWorker(value, cont, env, engine);
}

void UndefineWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        value->mark();
        cont->mark();
        env->mark();
    }
}

void UndefineWorker::run()
{
    Worker::run();

    ScamValue test = env->check(value, false);
    if ( isError(test) ) {
        engine->handleError(test);
    }
    else if ( truth(test) ) {
        Continuation * c =
            standardMemoryManager.make<UndefineCont>(value, cont, env, engine);
        c->handleValue(makeNull());
    }
    else {
        ScamValue err = makeError("Symbol not found (%{0})", value);
        err->errorCategory() = envCategory;
        engine->handleError(err);
    }
}
