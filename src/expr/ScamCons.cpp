#include "expr/ScamCons.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ConsWorker.hpp"
#include "expr/MapWorker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamCons::ScamCons(ExprHandle car, ExprHandle cdr)
    : car(car)
    , cdr(cdr)
{
}

ScamCons * ScamCons::makeInstance(ExprHandle car, ExprHandle cdr)
{
    return new ScamCons(car, cdr);
}

void ScamCons::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        car->mark();
        cdr->mark();
    }
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

void ScamCons::eval(Continuation * cont, Env * env) const
{
    workQueueHelper<ConsWorker>(cont, env, car, cdr);
}

void ScamCons::mapEval(Continuation * cont, Env * env) const
{
    workQueueHelper<MapWorker>(cont, env, car, cdr);
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

bool ScamCons::equals(ConstExprHandle expr) const
{
    if ( ! expr->isCons() ) {
        return false;
    }
    ScamCons const * that = dynamic_cast<ScamCons const *>(expr);
    return (car->equals(that->car) && cdr->equals(that->cdr) );
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

ExprHandle ScamCons::nthcar(size_t n) const
{
    auto f = [=] () -> ExprHandle {
        return ExpressionFactory::makeError("Index ",
                                            n,
                                            " requested for ",
                                            toString());
    };

    ExprHandle rv;

    if ( 0 == n ) {
        rv = car;
    }
    else if ( cdr->isCons() ) {
        rv = cdr->nthcar(n-1);
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

ExprHandle ScamCons::nthcdr(size_t n) const
{
    auto f = [=] () -> ExprHandle {
        return ExpressionFactory::makeError("Index ",
                                            n,
                                            " requested for ",
                                            toString());
    };

    ExprHandle rv;

    if ( 0 == n ) {
        rv = cdr;
    }
    else if ( cdr->isCons() ) {
        rv = cdr->nthcdr(n-1);
        if ( rv->error() ) {
            rv = f();
        }
    }
    else if ( n >= 1 ) {
        rv = f();
    }
    else {
        rv = cdr;
    }

    return rv;
}

