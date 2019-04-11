
#include "prim/Cons.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

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
        stringstream s;
        s << "cons is expecting 2 parameters, got "
          << args->toString() << "\n";
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }
    else {
        ScamExpr * car = args->nthcar(0);
        ScamExpr * cdr = args->nthcar(1);
        ScamExpr * cons = ExpressionFactory::makeCons(car, cdr);
        cont->run(cons);
    }
}

