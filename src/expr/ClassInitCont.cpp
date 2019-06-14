#include "expr/ClassInitCont.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"

using namespace scam;
using namespace std;

/* This continuation seems *entirely* useless */

ClassInitCont::ClassInitCont(ScamValue instance,
                             Continuation * cont,
                             ScamEngine * engine)
    : Continuation("ClassInit", engine)
    , instance(instance)
    , cont(cont)
{
}

ClassInitCont * ClassInitCont::makeInstance(ScamValue instance,
                                            Continuation * cont,
                                            ScamEngine * engine)
{
    return new ClassInitCont(instance, cont, engine);
}

void ClassInitCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        instance->mark();
        cont->mark();
    }
}

void ClassInitCont::handleValue(ScamValue expr)
{
    Continuation::handleValue(expr);
    cont->handleValue(instance);
}
