#include "expr/TypePredicates.hpp"

#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

bool scam::isNull(const ScamData * data)
{
    return data->type == ScamData::Null;
}

bool scam::error(const ScamData * data)
{
    return data->type == ScamData::Error;
}

bool scam::truth(const ScamData * data)
{
    if ( isNull(data) ) {
        return false;
    }
    if ( ! isBoolean(data) ) {
        return true;
    }

    return BOOLVAL(data);
}

bool scam::isBoolean(const ScamData * data)
{
    return data->type == ScamData::Boolean;
}

bool scam::isChar(const ScamData * data)
{
    return data->type == ScamData::Character;
}

bool scam::isString(const ScamData * data)
{
    return data->type == ScamData::String;
}

bool scam::isSymbol(const ScamData * data)
{
    return data->type == ScamData::Symbol;
}

bool scam::isKeyword(const ScamData * data)
{
    return data->type == ScamData::Keyword;
}

bool scam::isNumeric(const ScamData * data)
{
    return 0 != (data->type & ScamData::Numeric);
}

bool scam::isExact(const ScamData * data)
{
    if ( ! isNumeric(data) ) {
        stringstream s;
        s << "Exactness has no meaning for <" << writeValue(data) << ">";
        throw ScamException(s.str());
    }

    return EXACT(data);
}

bool scam::isComplex(const ScamData * data)
{
    return ScamData::ComplexBit == (data->type & ScamData::ComplexBit);
}

bool scam::isPureComplex(const ScamData * data)
{
    return isComplex(data) && ! isReal(data);
}

bool scam::isReal(const ScamData * data)
{
    return ScamData::RealBit == (data->type & ScamData::RealBit);
}

bool scam::isRational(const ScamData * data)
{
    return ScamData::RationalBit == (data->type & ScamData::RationalBit);
}

bool scam::isInteger(const ScamData * data)
{
    return ScamData::IntegerBit == (data->type & ScamData::IntegerBit);
}

bool scam::isNaN(const ScamData * data)
{
    return ScamData::NaNBit == (data->type & ScamData::NaNBit);
}

bool scam::isNegInf(const ScamData * data)
{
    return ScamData::NegInfBit == (data->type & ScamData::NegInfBit);
}

bool scam::isPosInf(const ScamData * data)
{
    return ScamData::PosInfBit == (data->type & ScamData::PosInfBit);
}

bool scam::isSpecialNumeric(const ScamData * data)
{
    return 0 != (data->type & ScamData::SpecialNumeric);
}

bool scam::isNil(const ScamData * data)
{
    return data->type == ScamData::Nil;
}

bool scam::isCons(const ScamData * data)
{
    return data->type == ScamData::Cons;
}

bool scam::isList(const ScamData * data)
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

bool scam::isVector(const ScamData * data)
{
    return data->type == ScamData::Vector;
}

bool scam::isByteVector(const ScamData * data)
{
    return data->type == ScamData::ByteVector;
}

bool scam::isClosure(const ScamData * data)
{
    return 0 != (data->type & ScamData::Closure);
}

bool scam::isProcedure(const ScamData * data)
{
    return 0 != (data->type & ScamData::Procedure);
}

bool scam::isClass(const ScamData * data)
{
    return data->type == ScamData::Class;
}

bool scam::isInstance(const ScamData * data)
{
    return data->type == ScamData::Instance;
}

bool scam::isDict(const ScamData * data)
{
    return data->type == ScamData::Dict;
}

bool scam::isSpecialForm(const ScamData * data)
{
    return data->type == ScamData::SpecialForm;
}

bool scam::isPrimitive(const ScamData * data)
{
    return data->type == ScamData::Primitive;
}

bool scam::isApplicable(const ScamData * data)
{
    return 0 != (data->type & ScamData::Applicable);
}

bool scam::isContinuation(const ScamData * data)
{
    return data->type == ScamData::Cont;
}
