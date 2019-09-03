#include "prim/PairOps.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern ScamValue
    carCdrCommon(ScamValue args, Continuation * cont, const char * name);

    extern bool setCarCdrCommon(ScamValue args,
                                Continuation * cont,
                                const char * name,
                                ScamValue & pair,
                                ScamValue & obj);
}

void scam::applyCar(ScamValue args, Continuation * cont)
{
    ScamValue obj = carCdrCommon(args, cont, "car");
    if ( ! isNothing(obj) ) {
        ScamValue car = getCar(obj);
        cont->handleValue(car);
    }
}

void scam::applyCdr(ScamValue args, Continuation * cont)
{
    ScamValue obj = carCdrCommon(args, cont, "cdr");
    if ( ! isNothing(obj) ) {
        ScamValue cdr = getCdr(obj);
        cont->handleValue(cdr);
    }
}

void scam::applyCons(ScamValue args, Continuation * cont)
{
    ObjectParameter p0, p1;
    if ( argsToParms(args, "cons", p0, p1) ) {
        cont->handleValue(makePair(p0.value, p1.value));
    }
}

void scam::applySetCarX(ScamValue args, Continuation * cont)
{
    static const char * name = "set-car!";

    ScamValue pair, obj;
    if ( setCarCdrCommon(args, cont, name, pair, obj) ) {
        pair->carValue() = obj;
        cont->handleValue(makeNothing());
    }
}

void scam::applySetCdrX(ScamValue args, Continuation * cont)
{
    static const char * name = "set-cdr!";

    ScamValue pair, obj;
    if ( setCarCdrCommon(args, cont, name, pair, obj) ) {
        pair->cdrValue() = obj;
        cont->handleValue(makeNothing());
    }
}

namespace
{
    ScamValue
    carCdrCommon(ScamValue args, Continuation * cont, const char * name)
    {
        ScamValue rv = makeNothing();

        PairParameter p0;
        if ( argsToParms(args, name, p0) ) {
            rv = p0.value;
        }

        return rv;
    }

    bool setCarCdrCommon(ScamValue args,
                         Continuation * cont,
                         const char * name,
                         ScamValue & pair,
                         ScamValue & obj)
    {
        bool rv = false;

        PairParameter pPair;
        MutableParameter p0(pPair);
        ObjectParameter p1;
        if ( argsToParms(args, name, p0, p1) ) {
            pair = p0.value;
            obj  = p1.value;
            rv = true;
        }

        return rv;
    }
}
