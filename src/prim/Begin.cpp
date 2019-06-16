#include "prim/Begin.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

void scam::applyBegin(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    static const char * myName = "begin";

    if ( ! isList(args) ) {
        failedArgParseMessage(myName, "(expr*)", args, cont, engine);
        return;
    }

    const size_t count = length(args);
    if ( 0 == count ) {
        cont->handleValue(args);
    }
    else {
        ScamValue last = nthcar(args, count - 1);
        cont->handleValue(last);
    }
}

