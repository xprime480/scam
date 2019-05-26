#include "prim/IncludeCont.hpp"

#include "WorkQueue.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "input/IncludeParser.hpp"
#include "prim/IncludeWorker.hpp"

using namespace scam;
using namespace std;

IncludeCont::IncludeCont(IncludeParser * parser,
                         Continuation * cont,
                         ScamEngine * engine,
                         size_t nextIdx)
    : Continuation("Include")
    , parser(parser)
    , cont(cont)
    , engine(engine)
{
}

IncludeCont * IncludeCont::makeInstance(IncludeParser * parser,
                                        Continuation * cont,
                                        ScamEngine * engine,
                                        size_t nextIdx)
{
    return new IncludeCont(parser, cont, engine, nextIdx);
}

void IncludeCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        parser->mark();
        cont->mark();
    }
}

void IncludeCont::run(ScamValue expr)
{
    if ( error(expr) ) {
        cont->run(expr);
        return;
    }

    workQueueHelper<IncludeWorker>(parser, cont, engine, nextIdx);
}
