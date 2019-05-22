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
    : ScamExpr(ScamData::Cons)
{
    CAR(this) = car;
    CDR(this) = cdr;
}

ScamCons * ScamCons::makeInstance(ExprHandle car, ExprHandle cdr)
{
    return new ScamCons(car, cdr);
}

void ScamCons::eval(Continuation * cont, Env * env) const
{
    workQueueHelper<ConsWorker>(cont, env, CAR(this), CDR(this));
}

void ScamCons::mapEval(Continuation * cont, Env * env) const
{
    workQueueHelper<MapWorker>(cont, env, CAR(this), CDR(this));
}

bool ScamCons::equals(ConstExprHandle expr) const
{
    if ( ! expr->isCons() ) {
        return false;
    }

    return (CAR(this)->equals(CAR(expr)) && CDR(this)->equals(CDR(expr)));
}

ExprHandle ScamCons::getCar() const
{
    return CAR(this);
}

ExprHandle ScamCons::getCdr() const
{
    return CDR(this);
}

size_t ScamCons::length() const
{
    ExprHandle cdr = CDR(this);
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

    ExprHandle cdr = CDR(this);
    ExprHandle rv;

    if ( 0 == n ) {
        rv = CAR(this);
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

    ExprHandle cdr = CDR(this);
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

