#include "expr/TypePredicates.hpp"

#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

bool scam::isNull(ScamValue data)
{
    return data->type == ScamData::Null;
}

bool scam::error(ScamValue data)
{
    return data->type == ScamData::Error;
}

bool scam::truth(ScamValue data)
{
    if ( isNull(data) ) {
        return false;
    }
    if ( ! isBoolean(data) ) {
        return true;
    }

    return BOOLVAL(data);
}

bool scam::isBoolean(ScamValue data)
{
    return data->type == ScamData::Boolean;
}

bool scam::isChar(ScamValue data)
{
    return data->type == ScamData::Character;
}

bool scam::isString(ScamValue data)
{
    return data->type == ScamData::String;
}

bool scam::isSymbol(ScamValue data)
{
    return data->type == ScamData::Symbol;
}

bool scam::isKeyword(ScamValue data)
{
    return data->type == ScamData::Keyword;
}

bool scam::isNumeric(ScamValue data)
{
    return 0 != (data->type & ScamData::Numeric);
}

bool scam::isExact(ScamValue data)
{
    if ( ! isNumeric(data) ) {
        stringstream s;
        s << "Exactness has no meaning for <" << writeValue(data) << ">";
        throw ScamException(s.str());
    }

    return EXACT(data);
}

bool scam::isComplex(ScamValue data)
{
    return ScamData::ComplexBit == (data->type & ScamData::ComplexBit);
}

bool scam::isPureComplex(ScamValue data)
{
    return isComplex(data) && ! isReal(data);
}

bool scam::isReal(ScamValue data)
{
    return ScamData::RealBit == (data->type & ScamData::RealBit);
}

bool scam::isRational(ScamValue data)
{
    return ScamData::RationalBit == (data->type & ScamData::RationalBit);
}

bool scam::isInteger(ScamValue data)
{
    return ScamData::IntegerBit == (data->type & ScamData::IntegerBit);
}

bool scam::isNaN(ScamValue data)
{
    return ScamData::NaNBit == (data->type & ScamData::NaNBit);
}

bool scam::isNegInf(ScamValue data)
{
    return ScamData::NegInfBit == (data->type & ScamData::NegInfBit);
}

bool scam::isPosInf(ScamValue data)
{
    return ScamData::PosInfBit == (data->type & ScamData::PosInfBit);
}

bool scam::isSpecialNumeric(ScamValue data)
{
    return 0 != (data->type & ScamData::SpecialNumeric);
}

bool scam::isNil(ScamValue data)
{
    return data->type == ScamData::Nil;
}

bool scam::isCons(ScamValue data)
{
    return data->type == ScamData::Cons;
}

bool scam::isList(ScamValue data)
{
    if ( isNil(data) ) {
        return true;
    }
    if ( isCons(data) ) {
        ScamValue cdr = CDR(data);
        return isList(cdr);
    }

    return false;
}

bool scam::isVector(ScamValue data)
{
    return data->type == ScamData::Vector;
}

bool scam::isByteVector(ScamValue data)
{
    return data->type == ScamData::ByteVector;
}

bool scam::isClosure(ScamValue data)
{
    return 0 != (data->type & ScamData::Closure);
}

bool scam::isProcedure(ScamValue data)
{
    return 0 != (data->type & ScamData::Procedure);
}

bool scam::isClass(ScamValue data)
{
    return data->type == ScamData::Class;
}

bool scam::isInstance(ScamValue data)
{
    return data->type == ScamData::Instance;
}

bool scam::isDict(ScamValue data)
{
    return data->type == ScamData::Dict;
}

bool scam::isSpecialForm(ScamValue data)
{
    return data->type == ScamData::SpecialForm;
}

bool scam::isPrimitive(ScamValue data)
{
    return data->type == ScamData::Primitive;
}

bool scam::isApplicable(ScamValue data)
{
    return 0 != (data->type & ScamData::Applicable);
}

bool scam::isContinuation(ScamValue data)
{
    return data->type == ScamData::Cont;
}
