#include "prim/Instantiator.hpp"

#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

#include <sstream>

using namespace std;
using namespace scam;

Instantiator::Instantiator(size_t & counter)
    : counter(counter)
    , dict(makeDict())
{
}

ScamValue Instantiator::exec(ScamValue value)
{
    return inst_value(value);
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
    else if ( isPair(expr) ) {
        return inst_pair(expr);
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
    ScamValue value = makeKeyword(s.str());
    ScamValue test = dictPut(dict, expr, value);
    if ( isError(test) ) {
        return test;
    }
    return value;
}

ScamValue Instantiator::inst_keyword(ScamValue expr)
{
    ScamValue test = dictHas(dict, expr);
    if ( isError(test) ) {
        return test;
    }
    else if ( truth(test) ) {
        return dictGet(dict, expr);
    }

    return new_mapping(expr);
}

ScamValue Instantiator::inst_pair(ScamValue expr)
{
    return map_pair(expr);
}

ScamValue Instantiator::inst_vector(ScamValue expr)
{
    return map_vector(expr);
}

ScamValue Instantiator::inst_dict(ScamValue expr)
{
    return map_dict(expr);
}
