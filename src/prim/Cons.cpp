#include "prim/Cons.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Cons::Cons()
    : Primitive("cons")
{
}

Cons * Cons::makeInstance()
{
    return new Cons();
}

void Cons::applyArgs(ExprHandle args, Continuation * cont)
{
    if ( args->error() ) {
        cont->run(args);
    }
    else if ( ! args->isList() || 2 != args->length() ) {
        ExprHandle err =
            ExpressionFactory::makeError("cons is expecting 2 parameters",
                                         ", got ",
                                         args->toString());
        cont->run(err);
    }
    else {
        ExprHandle car = args->nthcar(0);
        ExprHandle cdr = args->nthcar(1);
        ExprHandle cons = ExpressionFactory::makeCons(car, cdr);
        cont->run(cons);
    }
}

