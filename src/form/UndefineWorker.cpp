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

UndefineWorker::UndefineWorker(ScamValue value, Continuation * cont, Env * env)
    : Worker("Undefine")
    , value(value)
    , cont(cont)
    , env(env)
{
}

UndefineWorker *
UndefineWorker::makeInstance(ScamValue value, Continuation * cont, Env * env)
{
    return new UndefineWorker(value, cont, env);
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
        ScamEngine::getEngine().handleError(test);
    }
    else if ( truth(test) ) {
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Continuation * c = mm.make<UndefineCont>(value, cont, env);
        c->handleValue(makeNull());
    }
    else {
        ScamValue err = makeError("Symbol not found (%{0})", value);
        err->errorCategory() = envCategory;
        ScamEngine::getEngine().handleError(err);
    }
}
