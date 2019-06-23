#include "util/InstanceDef.hpp"

#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

InstanceDef::InstanceDef()
{
    reset();
}

void InstanceDef::mark() const
{
    name->mark();
    forms->mark();
}

ScamValue InstanceDef::transform(ScamValue args)
{
    reset();

    SymbolParameter p0;
    ScamValue rv = p0.transform(args);
    if ( ! isUnhandledError(rv) ) {
        ObjectParameter  pObj;
        CountedParameter p1(pObj);
        rv = p1.transform(rv);
        if ( ! isUnhandledError(rv) ) {
            valid = true;
            name  = p0.value;
            forms = p1.value;
        }
    }

    return rv;
}

void InstanceDef::reset()
{
    valid = false;
    name  = makeNothing();
    forms = makeNull();
}
