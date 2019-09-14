#include "prim/CommonError.hpp"

#include "value/ValueFactory.hpp"

using namespace scam;

ScamValue scam::make_common_error(const char * text)
{
    ScamValue msg = makeString(text);
    ScamValue rv  = makeBoolean(false);
    return makeList(rv, msg);
}
