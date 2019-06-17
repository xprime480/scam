#include "prim/IncludeCont.hpp"

#include "ScamEngine.hpp"
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
    : Continuation("Include", engine)
    , parser(parser)
    , cont(cont)
{
}

IncludeCont * IncludeCont::makeInstance(IncludeParser * parser,
                                        Continuation * cont,
                                        ScamEngine * engine,
                                        size_t nextIdx)
{
    return new IncludeCont(parser, cont, engine, nextIdx);
}

void IncludeCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        parser->mark();
        cont->mark();
    }
}

void IncludeCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isError(value) ) {
        engine->handleError(value);
        return;
    }

    workQueueHelper<IncludeWorker>(parser, cont, engine, nextIdx);
}
