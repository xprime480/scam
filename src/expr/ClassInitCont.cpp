#include "expr/ClassInitCont.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

ClassInitCont::ClassInitCont(ScamExpr * instance, Continuation * cont)
    : Continuation("ClassInit")
    , instance(instance)
    , cont(cont)
{
}

ClassInitCont * ClassInitCont::makeInstance(ScamExpr * instance,
                                            Continuation * cont)
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

void ClassInitCont::run(ScamExpr * expr)
{
    Continuation::run(expr);
    cont->run(instance);
}
