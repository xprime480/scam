#include "prim/IncludeWorker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "prim/IncludeCont.hpp"
#include "prim/Load.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

IncludeWorker::IncludeWorker(ExprHandle args,
                             Continuation * cont,
                             ScamEngine * engine)
    : Worker("Include")
    , args(args)
    , cont(cont)
    , engine(engine)
{
}

IncludeWorker * IncludeWorker::makeInstance(ExprHandle args,
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
    ExprHandle curr = args->getCar();
    ExprHandle newArg = ExpressionFactory::makeList(curr);
    ExprHandle rest = args->getCdr();

    Continuation * nextCont = cont;

    if ( ! rest->isNil() ) {
        nextCont = standardMemoryManager.make<IncludeCont>(rest, cont, engine);
    }

    Load * loader = standardMemoryManager.make<Load>(engine);
    loader->applyArgs(newArg, nextCont);
}
