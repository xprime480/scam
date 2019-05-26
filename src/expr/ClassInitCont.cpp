#include "expr/ClassInitCont.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"

using namespace scam;
using namespace std;

/* This continuation seems *entirely* useless */

ClassInitCont::ClassInitCont(ScamValue instance, Continuation * cont)
    : Continuation("ClassInit")
    , instance(instance)
    , cont(cont)
{
}

ClassInitCont *
ClassInitCont::makeInstance(ScamValue instance, Continuation * cont)
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

void ClassInitCont::run(ScamValue expr)
{
    Continuation::run(expr);
    cont->run(instance);
}
