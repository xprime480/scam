#include "prim/ListOps.hpp"

#include "Continuation.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applyMakeList(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine)
{
    CountParameter p0;
    ObjectParameter pObj;
    OptionalParameter p1(pObj);
    if ( argsToParms(args, engine, "make-list", p0, p1) ) {
        int count = asInteger(p0.value);
        ScamValue fill = makeNull();
        if ( ! isNothing(p1.value) ) {
            fill = p1.value;
        }

        ScamValue rv = makeNull();
        for ( int idx = 0 ; idx < count ; ++idx ) {
            rv = makePair(fill, rv);
        }

        cont->handleValue(rv);
    }
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
    ListParameter pList;
    ObjectParameter pObj;
    CountedParameter p0(pList);
    OptionalParameter p1(pObj);
    if ( argsToParms(args, engine, "append", p0, p1) ) {
        ScamValue objs = p0.value;
        ScamValue tail = makeNull();
        if ( ! isNothing(p1.value) ) {
            tail = p1.value;
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
}
