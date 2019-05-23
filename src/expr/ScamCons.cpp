#include "expr/ScamCons.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ConsWorker.hpp"
#include "expr/MapWorker.hpp"
#include "expr/TypePredicates.hpp"

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
    if ( ! TypePredicates::isCons(expr) ) {
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
    if ( TypePredicates::isCons(cdr) ) {
        len += cdr->length();
    }
    else if ( ! TypePredicates::isNil(cdr) ) {
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
                                            ExprWriter::write(this));
    };

    ExprHandle cdr = CDR(this);
    ExprHandle rv;

    if ( 0 == n ) {
        rv = CAR(this);
    }
    else if ( TypePredicates::isCons(cdr) ) {
        rv = cdr->nthcar(n-1);
        if ( TypePredicates::error(rv) ) {
            rv = f();
        }
    }
    else if ( TypePredicates::isNil(cdr) || n > 1 ) {
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
                                            ExprWriter::write(this));
    };

    ExprHandle cdr = CDR(this);
    ExprHandle rv;

    if ( 0 == n ) {
        rv = cdr;
    }
    else if ( TypePredicates::isCons(cdr) ) {
        rv = cdr->nthcdr(n-1);
        if ( TypePredicates::error(rv) ) {
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

