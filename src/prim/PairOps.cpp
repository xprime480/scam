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
                                  const char * name);

    extern bool setCarCdrCommon(ScamValue args,
                                Continuation * cont,
                                const char * name,
                                ScamValue & pair,
                                ScamValue & obj);
}

void scam::applyCar(ScamValue args,
                    Continuation * cont,
                    ScamEngine * engine)
{
    ScamValue obj = carCdrCommon(args, cont, "car");
    if ( ! isNothing(obj) ) {
        ScamValue car = getCar(obj);
        cont->run(car);
    }
}

void scam::applyCdr(ScamValue args,
                    Continuation * cont,
                    ScamEngine * engine)
{
    ScamValue obj = carCdrCommon(args, cont, "cdr");
    if ( ! isNothing(obj) ) {
        ScamValue car = getCdr(obj);
        cont->run(car);
    }
}

void scam::applyCons(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * name = "cons";

    ScamValue obj1, obj2;
    if ( ! getTwoObjs(args, cont, name, obj1, obj2) ) {
        return;
    }

    ScamValue pair = makePair(obj1, obj2);
    cont->run(pair);
}

void scam::applySetCarX(ScamValue args,
                        Continuation * cont,
                        ScamEngine * engine)
{
    static const char * name = "set-car!";

    ScamValue pair, obj;
    if ( setCarCdrCommon(args, cont, name, pair, obj) ) {
        CAR(pair) = obj;
        cont->run(makeNothing());
    }
}

void scam::applySetCdrX(ScamValue args,
                        Continuation * cont,
                        ScamEngine * engine)
{
    static const char * name = "set-cdr!";

    ScamValue pair, obj;
    if ( setCarCdrCommon(args, cont, name, pair, obj) ) {
        CDR(pair) = obj;
        cont->run(makeNothing());
    }
}

namespace
{
    ScamValue carCdrCommon(ScamValue args,
                           Continuation * cont,
                           const char * name)
    {
        ArgListHelper helper(args);

        ScamValue pair;
        if ( ! wantPair(name, helper, cont, pair) ) {
            return makeNothing();
        }
        if ( ! finishArgs(name, helper, cont) ) {
            return makeNothing();
        }
        return pair;
    }

    bool setCarCdrCommon(ScamValue args,
                         Continuation * cont,
                         const char * name,
                         ScamValue & pair,
                         ScamValue & obj)
    {
        ArgListHelper helper(args);

        if ( ! wantMutablePair(name, helper, cont, pair) ) {
            return false;
        }
        if ( ! wantObject(name, helper, cont, obj) ) {
            return false;
        }
        if ( ! finishArgs(name, helper, cont) ) {
            return false;
        }
        return true;
    }
}
