#include "prim/OutputOps.hpp"

#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "util/Parameter.hpp"

#include <iostream>

using namespace scam;
using namespace std;

void scam::applyDisplay(ScamValue args,
                        Continuation * cont,
                        ScamEngine * engine)
{
    ObjectParameter pObj;
    if ( argsToParms(args, engine, "newline", pObj) ) {
        ScamValue obj = pObj.value;

        if ( isString(obj) ) {
            cerr << asString(obj);
        }
        else {
            cerr << writeValue(obj);
        }
    }
}

void scam::applyNewline(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine)
{
    if ( argsToParms(args, engine, "newline") ) {
        cerr << "\n";
    }
}
