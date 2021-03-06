#include "expr/ClassInitCont.hpp"

#include "Continuation.hpp"
#include "value/ScamData.hpp"

using namespace scam;
using namespace std;

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

void ClassInitCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        instance->mark();
        cont->mark();
    }
}

void ClassInitCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);
    cont->handleValue(instance);
}
