#include "prim/VRef.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

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
        rv = ExpressionFactory::makeError("vref expects 2 argument, got ",
                                          args->length());
    }
    else {
        ScamExpr * indexArg = args->nthcar(0);
        ScamExpr * vecArg =  args->nthcar(1);

        if ( ! indexArg->isInteger() || indexArg->toInteger() < 0 ) {
            rv = ExpressionFactory::makeError("vref expects index ",
                                              "for argument 1, got: ",
                                              indexArg->toString());
        }
        else if ( ! vecArg->isVector() ) {
            rv = ExpressionFactory::makeError("vref expects vector ",
                                              "for argument 2, got: ",
                                              vecArg->toString());
        }
        else {
            int idx = indexArg->toInteger();
            rv = vecArg->nthcar((size_t) idx);
        }
    }

    cont->run(rv);

}


