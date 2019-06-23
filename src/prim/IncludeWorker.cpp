#include "prim/IncludeWorker.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "prim/IncludeCont.hpp"
#include "prim/Load.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

IncludeWorker::IncludeWorker(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
    : Worker("Include", engine)
    , args(args)
    , cont(cont)
{
}

IncludeWorker * IncludeWorker::makeInstance(ScamValue args,
                                            Continuation * cont,
                                            ScamEngine * engine)
{
    return new IncludeWorker(args, cont, engine);
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
        nextCont = standardMemoryManager.make<IncludeCont>(rest, cont, engine);
    }

    applyLoad(newArg, nextCont, engine);
}
