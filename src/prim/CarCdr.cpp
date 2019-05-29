#include "prim/CarCdr.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "input/SingletonParser.hpp"
#include "input/TypeParsers.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

ScamValue scam::carCdrCommon(ScamValue args,
                            Continuation * cont,
                            const char * name)
{
    ScamValue rv = makeNull();

    if ( error(args) ) {
        cont->run(args);
        return rv;
    }

    ConsParser * cp = standardMemoryManager.make<ConsParser>();
    SingletonParser * parser =
        standardMemoryManager.make<SingletonParser>(cp);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(name, "(a-cons)", args, cont);
        return rv;
    }

    rv = parser->get();
    return rv;
}

