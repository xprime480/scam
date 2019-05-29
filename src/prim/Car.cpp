#include "prim/Car.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "prim/CarCdr.hpp"

using namespace scam;
using namespace std;

void scam::applyCar(ScamValue args,
                    Continuation * cont,
                    ScamEngine * engine)
{
    ScamValue obj = carCdrCommon(args, cont, "car");
    if ( ! isNull(obj) ) {
        ScamValue car = getCar(obj);
        cont->run(car);
    }
}
