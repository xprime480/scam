#include "prim/IncludeCont.hpp"

#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "prim/IncludeWorker.hpp"

using namespace scam;
using namespace std;

IncludeCont::IncludeCont(ScamExpr * rest,
                         Continuation * cont,
                         ScamEngine * engine)
    : Continuation("Include")
    , rest(rest)
    , cont(cont)
    , engine(engine)
{
}

IncludeCont * IncludeCont::makeInstance(ScamExpr * rest,
                                        Continuation * cont,
                                        ScamEngine * engine)
{
    return new IncludeCont(rest, cont, engine);
}

void IncludeCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        rest->mark();
        cont->mark();
    }
}

void IncludeCont::run(ScamExpr * expr)
{
    if ( expr->error() ) {
        cont->run(expr);
        return;
    }

    workQueueHelper<IncludeWorker>(rest, cont, engine);
}
