#include "prim/Error.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "input/ListParser.hpp"
#include "util/ArgListHelper.hpp"

#include <sstream>

using namespace scam;
using namespace std;


void scam::applyError(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    static const char * name = "error";
    ArgListHelper helper(args);

    string str;
    ScamValue objs;
    if ( ! wantString(name, helper, cont, engine, str) ) {
        return;
    }
    if ( ! wantZeroPlus(name, helper, cont, engine, objs, isAnything) ) {
        return;
    }
    const char * msg { "Internal error: This should be literally impossible" };
    if ( ! finishArgs(name, helper, cont, engine, msg) ) {
        return;
    }

    stringstream s;
    unsigned len = length(objs);
    s << str;
    for ( unsigned i = 0 ; i < len ; ++i ) {
        s << " " << writeValue(nthcar(objs, i));
    }

    ScamValue err = makeError(s.str());
    engine->handleError(err);
}

