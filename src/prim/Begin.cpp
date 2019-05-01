#include "prim/Begin.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Begin::Begin()
    : Primitive("begin")
{
}

Begin * Begin::makeInstance()
{
    return new Begin();
}

void Begin::applyArgs(ExprHandle args, Continuation * cont)
{
    if ( args->isNil() ) {
        cont->run(args);
    }
    else {
        ExprHandle last = args->nthcar(args->length() - 1);
        cont->run(last);
    }
}

