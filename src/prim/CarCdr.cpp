#include "prim/CarCdr.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

CarCdr::CarCdr(char const * name)
    : Primitive(name)
{
}

void CarCdr::applyArgs(ExprHandle args, Continuation * cont)
{
    if ( args->error() ) {
        cont->run(args);
    }
    else if ( ! args->isList() ||
              1 != args->length() ||
              ! args->nthcar(0)->isCons() ) {
        ExprHandle err =
            ExpressionFactory::makeError(name,
                                         " is expecting a non-empty list, ",
                                         "got ",
                                         args->toString());
        cont->run(err);
    }
    else {
        finish(args, cont);
    }
}


