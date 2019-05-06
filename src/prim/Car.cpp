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

void Car::finish(ExprHandle cons, Continuation * cont)
{
    ExprHandle car = cons->getCar();
    cont->run(car);
}

