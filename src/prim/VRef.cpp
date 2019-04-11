
#include "prim/VRef.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

VRef::VRef()
    : Primitive("vref")
{
}

VRef * VRef::makeInstance()
{
    return new VRef();
}

void VRef::applyArgs(ScamExpr * args, Continuation * cont)
{
    ScamExpr * rv;

    if ( args->length() < 2u ) {
        stringstream s;
        s << "vref expects 2 argument, got " << args->length();
        rv = ExpressionFactory::makeError(s.str());
    }
    else {
        ScamExpr * indexArg = args->nthcar(0);
        ScamExpr * vecArg =  args->nthcar(1);

        if ( ! indexArg->isInteger() || indexArg->toInteger() < 0 ) {
            stringstream s;
            s << "vref expects index for argument 1, got: "
              << indexArg->toString();
            rv = ExpressionFactory::makeError(s.str());
        }
        else if ( ! vecArg->isVector() ) {
            stringstream s;
            s << "vref expects vector for argument 2, got: "
              << vecArg->toString();
            rv = ExpressionFactory::makeError(s.str());
        }
        else {
            int idx = indexArg->toInteger();
            rv = vecArg->nthcar((size_t) idx);
        }
    }

    cont->run(rv);

}


