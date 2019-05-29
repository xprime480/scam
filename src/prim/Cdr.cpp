#include "prim/Cdr.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "prim/CarCdr.hpp"

using namespace scam;
using namespace std;

void scam::applyCdr(ScamValue args,
                    Continuation * cont,
                    ScamEngine * engine)
{
    ScamValue obj = carCdrCommon(args, cont, "cdr");
    if ( ! isNull(obj) ) {
        ScamValue car = getCdr(obj);
        cont->run(car);
    }
}
