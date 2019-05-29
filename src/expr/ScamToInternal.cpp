#include "expr/ScamToInternal.hpp"

#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

char scam::asChar(ScamValue data)
{
    if ( ! isChar(data) ) {
        stringstream s;
        s << "Cannot convert <" << writeValue(data) << "> to character";
        throw ScamException(s.str());
    }

    return CHARVAL(data);
}

double scam::asDouble(ScamValue data)
{
    if ( ! isReal(data) ) {
        stringstream s;
        s << "Cannot convert <" << writeValue(data) << "> to double";
        throw ScamException(s.str());
    }

    if ( isInteger(data) ) {
        return (double) INTVAL(data);
    }
    else if ( isRational(data) ) {
        return ((double) NUMPART(data) / (double) DENPART(data) );
    }
    else if ( isSpecialNumeric(data) ) {
        // drop through to error case;
    }
    else if ( isReal(data) ) {
        return REALVAL(data);
    }

    return 0.0;
}

RationalPair scam::asRational(ScamValue data)
{
    if ( ! isRational(data) ) {
        stringstream s;
        s << "Cannot convert <" << writeValue(data) << "> to rational";
        throw ScamException(s.str());
    }

    RationalPair pair { 0, 1 };

    if ( isInteger(data) ) {
        pair.num = INTVAL(data);
    }
    else {
        pair.num = NUMPART(data);
        pair.den = DENPART(data);
    }

    return pair;
}

int scam::asInteger(ScamValue data)
{
    if ( ! isInteger(data) ) {
        stringstream s;
        s << "Cannot convert <" << writeValue(data) << "> to integer";
        throw ScamException(s.str());
    }

    return INTVAL(data);
}
