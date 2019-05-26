#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "util/NumericUtils.hpp"

#include <cctype>
#include <cmath>
#include <sstream>

using namespace scam;
using namespace std;

ScamValue scam::realPart(ScamValue data)
{
    if ( ! isNumeric(data) ) {
        stringstream s;
        s << "<" << writeValue(data) << "> is not numeric; has no real part";
        throw ScamException(s.str());
    }

    if ( isPureComplex(data) ) {
        return REALPART(data);
    }

    return data;
}

ScamValue scam::imagPart(ScamValue data)
{
    if ( ! isNumeric(data) ) {
        stringstream s;
        s << "<" << writeValue(data)
          << "> is not numeric; has no imaginary part";
        throw ScamException(s.str());
    }

    if ( isPureComplex(data) ) {
        return IMAGPART(data);
    }

    return makeInteger(0, true);
}

