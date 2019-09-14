#include "value/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "util/NumericUtils.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

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
        return data->realPart();
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
        return data->imagPart();
    }

    return makeInteger(0, true);
}

