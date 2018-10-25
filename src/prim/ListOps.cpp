
#include "prim/ListOps.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

List::List()
    : Primitive("list")
{
}

void List::applyArgs(ScamExpr * args, ContHandle cont)
{
    cont->run(args);
}

Cons::Cons()
    : Primitive("cons")
{
}

void Cons::applyArgs(ScamExpr * args, ContHandle cont)
{
    if ( args->error() ) {
        cont->run(args);
    }
    else if ( ! args->isList() || 2 != args->length() ) {
        stringstream s;
        s << "cons is expecting 2 parameters, got " << args->toString() << "\n";
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err.get());
    }
    else {
        ScamExpr * car = args->nthcar(0).get();
        ScamExpr * cdr = args->nthcar(1).get();
        ExprHandle cons = ExpressionFactory::makeCons(car, cdr);
        cont->run(cons.get());
    }
}

CarCdr::CarCdr(char const * name)
    : Primitive(name)
{
}

void CarCdr::applyArgs(ScamExpr * args, ContHandle cont)
{
    if ( args->error() ) {
        cont->run(args);
    }
    else if ( ! args->isList() || 1 != args->length() ||
              ! args->nthcar(0)->isCons() ) {
        stringstream s;
        s << name << " is expecting a non-empty list, got " << args->toString() << "\n";
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err.get());
    }
    else {
        finish(args, cont);
    }
}


Car::Car()
    : CarCdr("car")
{
}

void Car::finish(ScamExpr * args, ContHandle cont)
{
    ScamExpr * car = args->nthcar(0)->nthcar(0).get();
    cont->run(car);
}

Cdr::Cdr()
    : CarCdr("cdr")
{
}

void Cdr::finish(ScamExpr * args, ContHandle cont)
{
    ScamExpr * cdr = args->nthcar(0)->nthcdr(0).get();
    cont->run(cdr);
}
