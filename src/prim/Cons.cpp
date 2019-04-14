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

void Cons::applyArgs(ScamExpr * args, Continuation * cont)
{
    if ( args->error() ) {
        cont->run(args);
    }
    else if ( ! args->isList() || 2 != args->length() ) {
        ScamExpr * err =
            ExpressionFactory::makeError("cons is expecting 2 parameters",
                                         ", got ",
                                         args->toString());
        cont->run(err);
    }
    else {
        ScamExpr * car = args->nthcar(0);
        ScamExpr * cdr = args->nthcar(1);
        ScamExpr * cons = ExpressionFactory::makeCons(car, cdr);
        cont->run(cons);
    }
}

