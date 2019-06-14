#include "prim/ValueMapper.hpp"

#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace std;
using namespace scam;

ScamValue ValueMapper::map_dict(ScamValue expr)
{
    if ( 0u == length(expr) ) {
        return expr;
    }

    ScamValue rv   = makeDict();
    ScamValue dict = expr;

    KeyVec const & keys = getDictKeys(dict);
    for ( auto key : keys ) {
        ScamValue val = dictGet(dict, key);
        ScamValue newVal = map_value(val);
        if ( isError(newVal) ) {
            return newVal;
        }
        dictPut(rv, key, newVal);
    }

    return rv;
}

ScamValue ValueMapper::map_vector(ScamValue expr)
{
    ExprVec newExprs;
    size_t const len = length(expr);

    for ( size_t idx = 0 ; idx < len ; ++idx ) {
        ScamValue val = nthcar(expr, idx);
        ScamValue newVal = map_value(val);
        newExprs.push_back(newVal);
    }

    ScamValue rv = makeVector(newExprs);
    return rv;
}

ScamValue ValueMapper::map_pair(ScamValue expr)
{
    ScamValue head = nthcar(expr, 0);
    ScamValue tail = nthcdr(expr, 0);
    ScamValue newHead = map_value(head);
    ScamValue newTail = map_value(tail);
    return makePair(newHead, newTail);
}
