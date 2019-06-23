#include "util/FunctionDef.hpp"

#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

FunctionDef::FunctionDef()
    : fname(makeNothing())
{
}

void FunctionDef::mark() const
{
    fname->mark();
    lambda.mark();
}

ScamValue FunctionDef::transform(ScamValue args)
{
    valid = false;
    
    SymbolParameter  p0;
    ScamValue rv = p0.transform(args);
    if ( ! isUnhandledError(rv) ) {
        rv = lambda.transform(rv);
        if ( ! isUnhandledError(rv) ) {
            valid = true;
            fname = p0.value;
        }
    }

    return rv;
}
