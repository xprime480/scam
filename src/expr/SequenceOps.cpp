#include "expr/SequenceOps.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamValue scam::append(ScamValue expr, ScamValue tail)
{
    if ( ! isList(expr) ) {
        stringstream s;
        s << "ScamListAdapter expected a list, got: " << writeValue(expr);
        auto msg = s.str();
        throw ScamException(msg);
    }

    if ( isNil(expr) ) {
        return ExpressionFactory::makeList(tail);
    }

    /* by assumption, isCons! */

    ScamValue car = expr->nthcar(0);
    ScamValue cdr = expr->nthcdr(0);
    ScamValue newCdr = append(cdr, tail);
    return ExpressionFactory::makeCons(car, newCdr);
}
