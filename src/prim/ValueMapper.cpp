#include "prim/ValueMapper.hpp"

#include "expr/ScamExpr.hpp"
#include "expr/ScamDict.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"

using namespace std;
using namespace scam;

ScamValue ValueMapper::map_dict(ScamValue expr)
{
    if ( 0u == expr->length() ) {
        return expr;
    }

    ScamDict * rv   = ExpressionFactory::makeDict();
    ScamDict * dict = dynamic_cast<ScamDict *>(expr);

    KeyVec const & keys = dict->getKeys();
    for ( auto key : keys ) {
        ScamValue val = dict->get(key);
        ScamValue newVal = map_value(val);
        if ( TypePredicates::error(newVal) ) {
            return newVal;
        }
        rv->put(key, newVal);
    }

    return rv;
}

ScamValue ValueMapper::map_vector(ScamValue expr)
{
    ExprVec newExprs;
    size_t const len = expr->length();

    for ( size_t idx = 0 ; idx < len ; ++idx ) {
        ScamValue val = expr->nthcar(idx);
        ScamValue newVal = map_value(val);
        newExprs.push_back(newVal);
    }

    ScamValue rv = ExpressionFactory::makeVector(newExprs);
    return rv;
}

ScamValue ValueMapper::map_cons(ScamValue expr)
{
    ScamValue head = expr->nthcar(0);
    ScamValue tail = expr->nthcdr(0);
    ScamValue newHead = map_value(head);
    ScamValue newTail = map_value(tail);
    return ExpressionFactory::makeCons(newHead, newTail);
}
