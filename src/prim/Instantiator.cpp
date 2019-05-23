#include "prim/Instantiator.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "input/SingletonParser.hpp"

#include <sstream>

using namespace std;
using namespace scam;

Instantiator::Instantiator(size_t & counter)
    : counter(counter)
    , dict(ExpressionFactory::makeDict())
{
}

ScamValue Instantiator::exec(SingletonParser * parser)
{
    ScamValue expr = parser->get();
    return inst_value(expr);
}

ScamValue Instantiator::map_value(ScamValue val)
{
    return inst_value(val);
}

ScamValue Instantiator::inst_value(ScamValue expr)
{
    if ( isKeyword(expr) ) {
        return inst_keyword(expr);
    }
    else if ( isCons(expr) ) {
        return inst_cons(expr);
    }
    else if ( isVector(expr) ) {
        return inst_vector(expr);
    }
    else if ( isDict(expr) ) {
        return inst_dict(expr);
    }
    else {
        return expr;
    }
}

ScamValue Instantiator::new_mapping(ScamValue expr)
{
    stringstream s;
    s << ":kw" << ++counter;
    ScamValue value = ExpressionFactory::makeKeyword(s.str());
    dict->put(expr, value);
    return value;
}

ScamValue Instantiator::inst_keyword(ScamValue expr)
{
    if ( dict->has(expr) ) {
        return dict->get(expr);
    }

    return new_mapping(expr);
}

ScamValue Instantiator::inst_cons(ScamValue expr)
{
    return map_cons(expr);
}

ScamValue Instantiator::inst_vector(ScamValue expr)
{
    return map_vector(expr);
}

ScamValue Instantiator::inst_dict(ScamValue expr)
{
    return map_dict(expr);
}
