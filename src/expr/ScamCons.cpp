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
    CAR(data) = car;
    CDR(data) = cdr;
}

ScamCons * ScamCons::makeInstance(ExprHandle car, ExprHandle cdr)
{
    return new ScamCons(car, cdr);
}

void ScamCons::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        CAR(data)->mark();
        CDR(data)->mark();
    }
}

void ScamCons::eval(Continuation * cont, Env * env) const
{
    workQueueHelper<ConsWorker>(cont, env, CAR(data), CDR(data));
}

void ScamCons::mapEval(Continuation * cont, Env * env) const
{
    workQueueHelper<MapWorker>(cont, env, CAR(data), CDR(data));
}

bool ScamCons::equals(ConstExprHandle expr) const
{
    if ( ! expr->isCons() ) {
        return false;
    }
    ScamCons const * that = dynamic_cast<ScamCons const *>(expr);
    return (CAR(data)->equals(CAR(that->data)) &&
            CDR(data)->equals(CDR(that->data)) );
}

ExprHandle ScamCons::getCar() const
{
    return CAR(data);
}

ExprHandle ScamCons::getCdr() const
{
    return CDR(data);
}

size_t ScamCons::length() const
{
    ExprHandle cdr = CDR(data);
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

    ExprHandle cdr = CDR(data);
    ExprHandle rv;

    if ( 0 == n ) {
        rv = CAR(data);
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

    ExprHandle cdr = CDR(data);
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

