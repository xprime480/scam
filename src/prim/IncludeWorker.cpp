#include "prim/IncludeWorker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "prim/IncludeCont.hpp"
#include "prim/Load.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

IncludeWorker::IncludeWorker(ScamExpr * args,
                             Continuation * cont,
                             ScamEngine * engine)
    : Worker("Include")
    , args(args)
    , cont(cont)
    , engine(engine)
{
}

IncludeWorker * IncludeWorker::makeInstance(ScamExpr * args,
                                            Continuation * cont,
                                            ScamEngine * engine)
{
    return new IncludeWorker(args, cont, engine);
}

void IncludeWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
    }
}

void IncludeWorker::run()
{
    ScamExpr * curr = args->getCar();
    ScamExpr * newArg = ExpressionFactory::makeList(curr);
    ScamExpr * rest = args->getCdr();

    Continuation * nextCont = cont;

    if ( ! rest->isNil() ) {
        nextCont = standardMemoryManager.make<IncludeCont>(rest, cont, engine);
    }

    Load * loader = standardMemoryManager.make<Load>(engine);
    loader->applyArgs(newArg, nextCont);
}
