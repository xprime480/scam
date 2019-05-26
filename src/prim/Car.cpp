#include "prim/Car.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"

using namespace scam;
using namespace std;

Car::Car()
    : CarCdr("car")
{
}

Car * Car::makeInstance()
{
    return new Car();
}

void Car::finish(ScamValue cons, Continuation * cont)
{
    ScamValue car = getCar(cons);
    cont->run(car);
}
