#include "prim/ErrorOps.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "input/ListParser.hpp"
#include "util/ArgListHelper.hpp"

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
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    ScamData::VectorData irritants;
    unsigned len = length(objs);
    for ( unsigned i = 0 ; i < len ; ++i ) {
        irritants.push_back(nthcar(objs, i));
    }

    ScamValue err = makeError(str.c_str(), irritants);
    engine->handleError(err);
}

void scam::applyRaise(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    static const char * name = "raise";
    ArgListHelper helper(args);

    ScamValue obj;
    if ( ! wantObject(name, helper, cont, engine, obj) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    engine->handleError(obj);
}

