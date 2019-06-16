#include "prim/PairOps.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern ScamValue carCdrCommon(ScamValue args,
                                  Continuation * cont,
                                  ScamEngine * engine,
                                  const char * name);

    extern bool setCarCdrCommon(ScamValue args,
                                Continuation * cont,
                                ScamEngine * engine,
                                const char * name,
                                ScamValue & pair,
                                ScamValue & obj);
}

void scam::applyCar(ScamValue args,
                    Continuation * cont,
                    ScamEngine * engine)
{
    ScamValue obj = carCdrCommon(args, cont, engine, "car");
    if ( ! isNothing(obj) ) {
        ScamValue car = getCar(obj);
        cont->handleValue(car);
    }
}

void scam::applyCdr(ScamValue args,
                    Continuation * cont,
                    ScamEngine * engine)
{
    ScamValue obj = carCdrCommon(args, cont, engine, "cdr");
    if ( ! isNothing(obj) ) {
        ScamValue car = getCdr(obj);
        cont->handleValue(car);
    }
}

void scam::applyCons(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * name = "cons";

    ScamValue obj1, obj2;
    if ( ! getTwoObjs(args, cont, engine, name, obj1, obj2) ) {
        return;
    }

    ScamValue pair = makePair(obj1, obj2);
    cont->handleValue(pair);
}

void scam::applySetCarX(ScamValue args,
                        Continuation * cont,
                        ScamEngine * engine)
{
    static const char * name = "set-car!";

    ScamValue pair, obj;
    if ( setCarCdrCommon(args, cont, engine, name, pair, obj) ) {
        CAR(pair) = obj;
        cont->handleValue(makeNothing());
    }
}

void scam::applySetCdrX(ScamValue args,
                        Continuation * cont,
                        ScamEngine * engine)
{
    static const char * name = "set-cdr!";

    ScamValue pair, obj;
    if ( setCarCdrCommon(args, cont, engine, name, pair, obj) ) {
        CDR(pair) = obj;
        cont->handleValue(makeNothing());
    }
}

namespace
{
    ScamValue carCdrCommon(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine,
                           const char * name)
    {
        ArgListHelper helper(args);

        ScamValue pair;
        if ( ! wantPair(name, helper, cont, engine, pair) ) {
            return makeNothing();
        }
        if ( ! finishArgs(name, helper, cont, engine) ) {
            return makeNothing();
        }
        return pair;
    }

    bool setCarCdrCommon(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine,
                         const char * name,
                         ScamValue & pair,
                         ScamValue & obj)
    {
        ArgListHelper helper(args);

        if ( ! wantMutablePair(name, helper, cont, engine, pair) ) {
            return false;
        }
        if ( ! wantObject(name, helper, cont, engine, obj) ) {
            return false;
        }
        if ( ! finishArgs(name, helper, cont, engine) ) {
            return false;
        }
        return true;
    }
}
