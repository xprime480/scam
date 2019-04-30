#include "prim/Error.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Error::Error()
    : Primitive("error")
{
}

Error * Error::makeInstance()
{
    return new Error();
}

void Error::applyArgs(ExprHandle args, Continuation * cont)
{
    stringstream s;
    unsigned len = args->length();
    if ( 0 == len ) {
        s << "Error detected";
    }
    else if ( 1 == len ) {
        s << args->nthcar(0)->toString();
    }
    else {
        for ( unsigned i = 0 ; i < len ; ++i ) {
            s << "[" << (i+1) << "] "
              << args->nthcar(i)->toString() << "\n";
        }
    }

    ExprHandle err = ExpressionFactory::makeError(s.str());
    cont->run(err);
}
