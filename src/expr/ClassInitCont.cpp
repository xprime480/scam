#include "expr/ClassInitCont.hpp"

#include "Continuation.hpp"
#include "expr/ScamInstance.hpp"

using namespace scam;
using namespace std;

/* This continuation seems *entirely* useless */

ClassInitCont::ClassInitCont(ScamInstance * instance, Continuation * cont)
    : Continuation("ClassInit")
    , instance(instance)
    , cont(cont)
{
}

ClassInitCont *
ClassInitCont::makeInstance(ScamInstance * instance, Continuation * cont)
{
    return new ClassInitCont(instance, cont);
}

void ClassInitCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        instance->mark();
        cont->mark();
    }
}

void ClassInitCont::run(ExprHandle expr)
{
    Continuation::run(expr);
    cont->run(instance);
}
