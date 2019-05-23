#include "prim/SpawnWorker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

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

void SpawnWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        cont->mark();
    }
}

void SpawnWorker::run()
{
    ScamValue flag = ExpressionFactory::makeBoolean(value);
    cont->run(flag);
}
