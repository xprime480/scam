#include "prim/ValueMapper.hpp"

#include "expr/ScamExpr.hpp"
#include "expr/ScamDict.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace std;
using namespace scam;

ScamExpr * ValueMapper::map_dict(ScamExpr * expr)
{
    if ( 0u == expr->length() ) {
        return expr;
    }

    ScamDict * rv   = ExpressionFactory::makeDict();
    ScamDict * dict = dynamic_cast<ScamDict *>(expr);

    KeyVec const & keys = dict->getKeys();
    for ( auto key : keys ) {
        ScamExpr * val = dict->get(key);
        ScamExpr * newVal = map_value(val);
        if ( newVal->error() ) {
            return newVal;
        }
        rv->put(key, newVal);
    }

    return rv;
}

ScamExpr * ValueMapper::map_vector(ScamExpr * expr)
{
    ExprVec newExprs;
    size_t const len = expr->length();

    for ( size_t idx = 0 ; idx < len ; ++idx ) {
        ScamExpr * val = expr->nthcar(idx);
        ScamExpr * newVal = map_value(val);
        newExprs.push_back(newVal);
    }

    ScamExpr * rv = ExpressionFactory::makeVector(newExprs);
    return rv;
}

ScamExpr * ValueMapper::map_cons(ScamExpr * expr)
{
    ScamExpr * head = expr->nthcar(0);
    ScamExpr * tail = expr->nthcdr(0);
    ScamExpr * newHead = map_value(head);
    ScamExpr * newTail = map_value(tail);
    return ExpressionFactory::makeCons(newHead, newTail);
}
