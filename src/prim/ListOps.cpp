#include "prim/ListOps.hpp"

#include "Continuation.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

void scam::applyMakeList(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine)
{
    static const char * name { "make-list" };
    ArgListHelper helper(args);

    int count;
    if ( ! wantNonNegativeInteger(name, helper, cont, engine, count) ) {
        return;
    }
    ScamValue fill = wantOptional<ScamValue>(name,
                                             helper,
                                             cont,
                                             isAnything,
                                             identity,
                                             makeNull());
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    ScamValue rv = makeNull();
    for ( int idx = 0 ; idx < count ; ++idx ) {
        rv = makePair(fill, rv);
    }

    cont->handleValue(rv);
}

void scam::applyList(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    /** literally anything goes **/
    cont->handleValue(args);
}

void scam::applyAppend(ScamValue args,
                       Continuation * cont,
                       ScamEngine * engine)
{
    static const char * name { "append" };
    ArgListHelper helper(args);

    ScamValue objs;
    if ( ! wantZeroPlus(name, helper, cont, engine, objs, isList) ) {
        return;
    }
    ScamValue tail = wantOptional<ScamValue>(name,
                                             helper,
                                             cont,
                                             isAnything,
                                             identity,
                                             makeNull());
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    ScamValue rv = tail;
    for ( int idx = length(objs) ; idx > 0 ; /**/ ) {
        --idx;
        ScamValue xs = nthcar(objs, idx);
        for ( int idx2 = length(xs) ; idx2 > 0 ; /**/ ) {
            --idx2;
            ScamValue x = nthcar(xs, idx2);
            rv = makePair(x, rv);
        }
    }

    cont->handleValue(rv);
}
