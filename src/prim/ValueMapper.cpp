#include "prim/ValueMapper.hpp"

#include "expr/ScamExpr.hpp"
#include "expr/ScamDict.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace std;
using namespace scam;

ExprHandle ValueMapper::map_dict(ExprHandle expr)
{
    if ( 0u == expr->length() ) {
        return expr;
    }

    ScamDict * rv   = ExpressionFactory::makeDict();
    ScamDict * dict = dynamic_cast<ScamDict *>(expr);

    KeyVec const & keys = dict->getKeys();
    for ( auto key : keys ) {
        ExprHandle val = dict->get(key);
        ExprHandle newVal = map_value(val);
        if ( newVal->error() ) {
            return newVal;
        }
        rv->put(key, newVal);
    }

    return rv;
}

ExprHandle ValueMapper::map_vector(ExprHandle expr)
{
    ExprVec newExprs;
    size_t const len = expr->length();

    for ( size_t idx = 0 ; idx < len ; ++idx ) {
        ExprHandle val = expr->nthcar(idx);
        ExprHandle newVal = map_value(val);
        newExprs.push_back(newVal);
    }

    ExprHandle rv = ExpressionFactory::makeVector(newExprs);
    return rv;
}

ExprHandle ValueMapper::map_cons(ExprHandle expr)
{
    ExprHandle head = expr->nthcar(0);
    ExprHandle tail = expr->nthcdr(0);
    ExprHandle newHead = map_value(head);
    ExprHandle newTail = map_value(tail);
    return ExpressionFactory::makeCons(newHead, newTail);
}
