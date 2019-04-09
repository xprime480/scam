
#include "prim/Misc.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

Progn::Progn()
    : Primitive("progn")
{
}

Progn * Progn::makeInstance()
{
    return new Progn();
}

void Progn::applyArgs(ScamExpr * args, Continuation * cont)
{
    if ( args->isNil() ) {
        cont->run(args);
    }
    else {
        ScamExpr * last = args->nthcar(args->length() - 1);
        cont->run(last);
    }
}

