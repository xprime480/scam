#include "prim/SpawnWorker.hpp"

#include "Continuation.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

SpawnWorker::SpawnWorker(Continuation * cont, bool value)
    : Worker("SpawnWorker")
    , cont(cont)
    , value(value)
{
}

SpawnWorker * SpawnWorker::makeInstance(Continuation * cont, bool value)
{
    return new SpawnWorker(cont, value);
}

void SpawnWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        cont->mark();
    }
}

void SpawnWorker::run()
{
    Worker::run();

    ScamValue flag = makeBoolean(value);
    cont->handleValue(flag);
}
