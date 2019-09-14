#include "prim/IncludeCont.hpp"

#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "prim/IncludeWorker.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"

using namespace scam;
using namespace std;

IncludeCont::IncludeCont(ScamValue args, Continuation * cont)
    : Continuation("Include")
    , args(args)
    , cont(cont)
{
}

IncludeCont * IncludeCont::makeInstance(ScamValue args, Continuation * cont)
{
    return new IncludeCont(args, cont);
}

void IncludeCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
    }
}

void IncludeCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        workQueueHelper<IncludeWorker>(args, cont);
    }
}
