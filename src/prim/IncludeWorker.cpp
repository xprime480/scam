#include "prim/IncludeWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "prim/IncludeCont.hpp"
#include "prim/Load.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

IncludeWorker::IncludeWorker(ScamValue args, Continuation * cont)
    : Worker("Include")
    , args(args)
    , cont(cont)
{
}

IncludeWorker * IncludeWorker::makeInstance(ScamValue args, Continuation * cont)
{
    return new IncludeWorker(args, cont);
}

void IncludeWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
    }
}

void IncludeWorker::run()
{
    ScamValue curr = getCar(args);
    ScamValue newArg = makeList(curr);

    Continuation * nextCont = cont;
    ScamValue rest = getCdr(args);
    if ( ! isNull(rest) ) {
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        nextCont = mm.make<IncludeCont>(rest, cont);
    }

    applyLoad(newArg, nextCont);
}
