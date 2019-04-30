#include "prim/Progn.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"

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

void Progn::applyArgs(ExprHandle args, Continuation * cont)
{
    if ( args->isNil() ) {
        cont->run(args);
    }
    else {
        ExprHandle last = args->nthcar(args->length() - 1);
        cont->run(last);
    }
}

