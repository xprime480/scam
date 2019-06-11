#include "prim/Cons.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "input/CountedListParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

void scam::applyCons(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * myName = "cons";
    if ( error(args) ) {
        cont->run(args);
    }

    CountedListParser * parser = getCountedListOfAnythingParser(2, 2);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form form)", args, cont);
    }
    else {
        ScamValue car = parser->get(0);
        ScamValue cdr = parser->get(1);
        ScamValue pair = makePair(car, cdr);
        cont->run(pair);
    }
}

