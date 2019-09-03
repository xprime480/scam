#include "form/LetBaseWorker.hpp"

#include "Continuation.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "util/LetDef.hpp"

using namespace scam;
using namespace std;

LetBaseWorker::LetBaseWorker(char const * name,
                             LetDef & def,
                             Continuation * cont,
                             Env * env)
    : Worker(name)
    , cont(cont)
    , env(env)
    , def(def)
{
}

void LetBaseWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        def.mark();
        cont->mark();
        env->mark();
    }
}

void LetBaseWorker::run()
{
    Worker::run();

    do_next(def.formals, def.values, def.forms);
}
