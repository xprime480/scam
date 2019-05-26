#include "prim/Error.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/SequenceOps.hpp"
#include "input/ListParser.hpp"
#include "util/ArgListHelper.hpp"

#include <sstream>

using namespace scam;
using namespace std;

static const char * myName = "error";

Error::Error()
    : Primitive(myName)
{
}

Error * Error::makeInstance()
{
    return new Error();
}

void Error::applyArgs(ScamValue args, Continuation * cont)
{
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont);
        return;
    }

    stringstream s;
    unsigned len = length(args);
    if ( 0 == len ) {
        s << "Error detected";
    }
    else if ( 1 == len ) {
        s << writeValue(nthcar(args, 0));
    }
    else {
        for ( unsigned i = 0 ; i < len ; ++i ) {
            s << "[" << (i+1) << "] "
              << writeValue(nthcar(args, i)) << "\n";
        }
    }

    ScamValue err = ExpressionFactory::makeError(s.str());
    cont->run(err);
}
