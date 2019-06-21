#include "prim/OutputOps.hpp"

#include "Continuation.hpp"
#include "expr/ValueFactory.hpp"
#include "util/ArgListHelper.hpp"

#include <iostream>

using namespace scam;
using namespace std;

void scam::applyDisplay(ScamValue args,
                        Continuation * cont,
                        ScamEngine * engine)
{
    static const char * name { "display" };
    ArgListHelper helper(args);

    ScamValue value;
    if ( ! wantObject(name, helper, cont, engine, value) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    if ( isString(value) ) {
        cerr << asString(value);
    }
    else {
        cerr << writeValue(value);
    }

    cont->handleValue(makeNothing());
}

void scam::applyNewline(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine)
{
    static const char * name { "newline" };
    ArgListHelper helper(args);

    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    cerr << "\n";
    cont->handleValue(makeNothing());
}
