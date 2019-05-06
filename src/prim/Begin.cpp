#include "prim/Begin.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "begin";

Begin::Begin()
    : Primitive(myName)
{
}

Begin * Begin::makeInstance()
{
    return new Begin();
}

void Begin::applyArgs(ExprHandle args, Continuation * cont)
{
    if ( ! args->isList() ) {
        failedArgParseMessage(myName, "(expr*)", args, cont);
        return;
    }

    const size_t count = args->length();
    if ( 0 == count ) {
        cont->run(args);
    }
    else {
        ExprHandle last = args->nthcar(count - 1);
        cont->run(last);
    }
}

