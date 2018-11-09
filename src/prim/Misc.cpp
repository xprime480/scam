
#include "prim/Misc.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

Progn::Progn()
    : Primitive("progn")
{
}

void Progn::applyArgs(ScamExpr * args, ContHandle cont)
{
    if ( args->isNil() ) {
        cont->run(args);
    }
    else {
        ExprHandle last = args->nthcar(args->length() - 1);
        cont->run(last.get());
    }
}
