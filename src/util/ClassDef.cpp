#include "util/ClassDef.hpp"

#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

ClassDef::ClassDef()
    : base(makeNothing())
    , vars(makeNull())
{
}

void ClassDef::mark() const
{
    base->mark();
    vars->mark();
    for ( const auto & m : methods ) {
        m.mark();
    }
}

ScamValue ClassDef::transform(ScamValue args)
{
    base = makeNothing();
    vars = makeNull();
    methods.clear();
    valid = false;

    ScamValue rv = args;

    SymbolParameter  p0;
    SymbolParameter  pSym;
    ListOfParameter  p1(pSym);

    rv = p0.transform(args);
    if ( ! isUnhandledError(rv) ) {
        rv = p1.transform(rv);
        if ( ! isUnhandledError(rv) ) {
            rv = transformMethods(rv);
            if ( ! isUnhandledError(rv) ) {
                valid = true;
                base = p0.value;
                vars = p1.value;
            }
        }
    }

    return rv;
}

ScamValue ClassDef::transformMethods(ScamValue defs)
{
    std::vector<FunctionDef> ms;
    ScamValue rv = makeNull();

    while ( ! isNull(defs) ) {
        FunctionDef p;
        ScamValue next = getCar(defs);
        rv = p.transform(next);
        if ( isUnhandledError(rv) ) {
            break;
        }

        ms.push_back(p);
        defs = getCdr(defs);
    }

    if ( ! isUnhandledError(rv) ) {
        methods.swap(ms);
    }

    return rv;
}
