#include "prim/SpawnWorker.hpp"

#include "Continuation.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

SpawnWorker::SpawnWorker(Continuation * cont, ScamEngine * engine, bool value)
    : Worker("SpawnWorker", engine)
    , cont(cont)
    , value(value)
{
}

SpawnWorker *
SpawnWorker::makeInstance(Continuation * cont, ScamEngine * engine, bool value)
{
    return new SpawnWorker(cont, engine, value);
}

void SpawnWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        cont->mark();
    }
}

void SpawnWorker::run()
{
    ScamValue flag = makeBoolean(value);
    cont->run(flag);
}
