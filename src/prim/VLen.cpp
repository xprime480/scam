
#include "prim/VLen.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

VLen::VLen()
    : Primitive("vlen")
{
}

VLen * VLen::makeInstance()
{
    return new VLen();
}

void VLen::applyArgs(ScamExpr * args, Continuation * cont)
{
    ScamExpr * rv;

    if ( 0 == args->length() ) {
        rv = ExpressionFactory::makeError("vlen expects 1 argument, got none");
    }
    else {
        ScamExpr * arg = args->nthcar(0);
        if ( arg->isVector() ) {
            size_t len = arg->length();
            rv = ExpressionFactory::makeInteger(len);
        }
        else {
            stringstream s;
            s << "vlen expects vector argument, got: "
              << arg->toString();
            rv = ExpressionFactory::makeError(s.str());
        }
    }

    cont->run(rv);
}
