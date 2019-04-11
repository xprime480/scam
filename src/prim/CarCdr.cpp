
#include "prim/CarCdr.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <sstream>

using namespace scam;
using namespace std;

CarCdr::CarCdr(char const * name)
    : Primitive(name)
{
}

void CarCdr::applyArgs(ScamExpr * args, Continuation * cont)
{
    if ( args->error() ) {
        cont->run(args);
    }
    else if ( ! args->isList() || 1 != args->length() ||
              ! args->nthcar(0)->isCons() ) {
        stringstream s;
        s << name << " is expecting a non-empty list, got "
          << args->toString() << "\n";
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }
    else {
        finish(args, cont);
    }
}


