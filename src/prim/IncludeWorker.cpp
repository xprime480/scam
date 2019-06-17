#include "prim/IncludeWorker.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/ValueFactory.hpp"
#include "input/IncludeParser.hpp"
#include "prim/IncludeCont.hpp"
#include "prim/Load.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

IncludeWorker::IncludeWorker(IncludeParser * parser,
                             Continuation * cont,
                             ScamEngine * engine,
                             size_t idx)
    : Worker("Include", engine)
    , parser(parser)
    , cont(cont)
    , idx(idx)
{
}

IncludeWorker * IncludeWorker::makeInstance(IncludeParser * parser,
                                            Continuation * cont,
                                            ScamEngine * engine,
                                            size_t idx)
{
    return new IncludeWorker(parser, cont, engine, idx);
}

void IncludeWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        parser->mark();
        cont->mark();
    }
}

void IncludeWorker::run()
{
    const size_t count = parser->size();
    ScamValue curr = parser->get(idx);
    ScamValue newArg = makeList(curr);

    size_t nextIdx = idx + 1;
    Continuation * nextCont = cont;
    if ( nextIdx < count ) {
        nextCont = standardMemoryManager.make<IncludeCont>(parser,
                                                           cont,
                                                           engine,
                                                           nextIdx);
    }

    applyLoad(newArg, nextCont, engine);
}
