
#include "expr/ScamCons.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include "impl/ConsHelper.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace scam::cons_impl;
using namespace std;

ScamCons::ScamCons(ScamExpr * car, ScamExpr * cdr)
    : car(car->clone())
    , cdr(cdr->clone())
{
}

string ScamCons::toString() const
{
    stringstream s;
    s << "(";
    s << car->toString();
    ExprHandle next = cdr;
    while ( ! next->isNil() ) {
        if ( next->isCons() ) {
            s << " " << next->getCar()->toString();
            next = next->getCdr();
        }
        else {
            s << " . " << next->toString();
            break;
        }
    }
    s << ")";

    return s.str();
}

void ScamCons::eval(ContHandle cont, Env env)
{
    scamConsEvalHelper(car.get(), cdr.get(), cont, env);
}

void ScamCons::mapEval(ContHandle cont, Env env)
{
    scamConsMapHelper(car.get(), cdr.get(), cont, env);
}

bool ScamCons::isCons() const
{
    return true;
}

bool ScamCons::isList() const
{
    if ( cdr->isNil() ) {
        return true;
    }
    if ( ! cdr->isCons() ) {
        return false;
    }
    return cdr->isList();
}

ExprHandle ScamCons::getCar() const
{
    return car;
}

ExprHandle ScamCons::getCdr() const
{
    return cdr;
}

size_t ScamCons::length() const
{
    size_t len = 1;
    if ( cdr->isCons() ) {
        len += cdr->length();
    }
    else if ( ! cdr->isNil() ) {
        len += 1;
    }

    return len;
}

ExprHandle ScamCons::nth(size_t n) const
{
    auto f = [=] () -> ExprHandle {
        stringstream s;
        s << "Index " << n << " requested for " << toString();
        return ExpressionFactory::makeError(s.str());
    };

    ExprHandle rv;

    if ( 0 == n ) {
        rv = car;
    }
    else if ( cdr->isCons() ) {
        rv = cdr->nth(n-1);
        if ( rv->error() ) {
            rv = f();
        }
    }
    else if ( cdr->isNil() || n > 1 ) {
        rv = f();
    }
    else {
        rv = cdr;
    }

    return rv;
}
