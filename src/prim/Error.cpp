#include "prim/Error.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
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

void Error::applyArgs(ExprHandle args, Continuation * cont)
{
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont);
        return;
    }

    stringstream s;
    unsigned len = args->length();
    if ( 0 == len ) {
        s << "Error detected";
    }
    else if ( 1 == len ) {
        s << ExprWriter::write(args->nthcar(0));
    }
    else {
        for ( unsigned i = 0 ; i < len ; ++i ) {
            s << "[" << (i+1) << "] "
              << ExprWriter::write(args->nthcar(i)) << "\n";
        }
    }

    ExprHandle err = ExpressionFactory::makeError(s.str());
    cont->run(err);
}
