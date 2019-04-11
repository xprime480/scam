
#include "prim/Car.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

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

void Car::finish(ScamExpr * args, Continuation * cont)
{
    ScamExpr * car = args->nthcar(0)->nthcar(0);
    cont->run(car);
}

