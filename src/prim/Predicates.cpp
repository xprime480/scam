
#include "prim/Predicates.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

NilP::NilP()
    : Primitive("nil?")
{
}

void NilP::applyArgs(ScamExpr * args, ContHandle cont)
{
    ExprHandle rv;
    if ( 1 == args->length() ) {
        ExprHandle arg = args->nthcar(0);
        rv = ExpressionFactory::makeBoolean(arg->isNil());
    }
    else {
        stringstream s;
        s << "nil? expected single argument, got " << args->toString();
        rv = ExpressionFactory::makeError(s.str());
    }

    cont->run(rv.get());
}
