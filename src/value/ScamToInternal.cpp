#include "value/ScamToInternal.hpp"

#include "ScamException.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

char scam::asBool(ScamValue data)
{
    if ( ! isBoolean(data) ) {
        stringstream s;
        s << "Cannot convert <" << writeValue(data) << "> to boolean";
        throw ScamException(s.str());
    }

    return data->boolValue();
}

char scam::asChar(ScamValue data)
{
    if ( ! isChar(data) ) {
        stringstream s;
        s << "Cannot convert <" << writeValue(data) << "> to character";
        throw ScamException(s.str());
    }

    return data->charValue();
}

std::string scam::asString(ScamValue data)
{
    if ( ! isString(data) ) {
        stringstream s;
        s << "Cannot convert <" << writeValue(data) << "> to string";
        throw ScamException(s.str());
    }

    return data->stringValue();
}

double scam::asDouble(ScamValue data)
{
    if ( ! isReal(data) ) {
        stringstream s;
        s << "Cannot convert <" << writeValue(data) << "> to double";
        throw ScamException(s.str());
    }

    if ( isInteger(data) ) {
        return (double) data->intPart();
    }
    else if ( isRational(data) ) {
        return ((double) data->numPart() / (double) data->denPart() );
    }
    else if ( isSpecialNumeric(data) ) {
        // drop through to error case;
    }
    else if ( isReal(data) ) {
        return data->realValue();
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
        pair.num = data->intPart();
    }
    else {
        pair.num = data->numPart();
        pair.den = data->denPart();
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

    return data->intPart();
}

ScamPort * scam::asPort(ScamValue data)
{
    if ( ! isPort(data) ) {
        stringstream s;
        s << "Cannot convert <" << writeValue(data) << "> to port";
        throw ScamException(s.str());
    }

    return data->portValue();
}

Env * scam::asEnv(ScamValue data)
{
    if ( ! isEnv(data) ) {
        stringstream s;
        s << "Cannot convert <" << writeValue(data) << "> to env";
        throw ScamException(s.str());
    }

    return data->envValue();
}

