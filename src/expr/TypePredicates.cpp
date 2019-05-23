#include "expr/TypePredicates.hpp"

#include "ScamException.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ScamData.hpp"

#include <sstream>

using namespace scam;
using namespace std;

bool TypePredicates::isNull(const ScamData * data)
{
    return data->type == ScamData::Null;
}

bool TypePredicates::error(const ScamData * data)
{
    return data->type == ScamData::Error;
}

bool TypePredicates::truth(const ScamData * data)
{
    if ( isNull(data) ) {
        return false;
    }
    if ( ! isBoolean(data) ) {
        return true;
    }

    return BOOLVAL(data);
}

bool TypePredicates::isBoolean(const ScamData * data)
{
    return data->type == ScamData::Boolean;
}

bool TypePredicates::isChar(const ScamData * data)
{
    return data->type == ScamData::Character;
}

bool TypePredicates::isString(const ScamData * data)
{
    return data->type == ScamData::String;
}

bool TypePredicates::isSymbol(const ScamData * data)
{
    return data->type == ScamData::Symbol;
}

bool TypePredicates::isKeyword(const ScamData * data)
{
    return data->type == ScamData::Keyword;
}

bool TypePredicates::isNumeric(const ScamData * data)
{
    return 0 != (data->type & ScamData::Numeric);
}

bool TypePredicates::isExact(const ScamData * data)
{
    if ( ! isNumeric(data) ) {
        stringstream s;
        s << "Exactness has no meaning for <" << ExprWriter::write(data) << ">";
        throw ScamException(s.str());
    }

    return EXACT(data);
}

bool TypePredicates::isComplex(const ScamData * data)
{
    return ScamData::ComplexBit == (data->type & ScamData::ComplexBit);
}

bool TypePredicates::isPureComplex(const ScamData * data)
{
    return isComplex(data) && ! isReal(data);
}

bool TypePredicates::isReal(const ScamData * data)
{
    return ScamData::RealBit == (data->type & ScamData::RealBit);
}

bool TypePredicates::isRational(const ScamData * data)
{
    return ScamData::RationalBit == (data->type & ScamData::RationalBit);
}

bool TypePredicates::isInteger(const ScamData * data)
{
    return ScamData::IntegerBit == (data->type & ScamData::IntegerBit);
}

bool TypePredicates::isNaN(const ScamData * data)
{
    return ScamData::NaNBit == (data->type & ScamData::NaNBit);
}

bool TypePredicates::isNegInf(const ScamData * data)
{
    return ScamData::NegInfBit == (data->type & ScamData::NegInfBit);
}

bool TypePredicates::isPosInf(const ScamData * data)
{
    return ScamData::PosInfBit == (data->type & ScamData::PosInfBit);
}

bool TypePredicates::isNil(const ScamData * data)
{
    return data->type == ScamData::Nil;
}

bool TypePredicates::isCons(const ScamData * data)
{
    return data->type == ScamData::Cons;
}

bool TypePredicates::isList(const ScamData * data)
{
    if ( isNil(data) ) {
        return true;
    }
    if ( isCons(data) ) {
        const ScamData * cdr = (ScamData *) CDR(data);
        return isList(cdr);
    }

    return false;
}

bool TypePredicates::isVector(const ScamData * data)
{
    return data->type == ScamData::Vector;
}

bool TypePredicates::isByteVector(const ScamData * data)
{
    return data->type == ScamData::ByteVector;
}

bool TypePredicates::isProcedure(const ScamData * data)
{
    return 0 != (data->type & ScamData::Procedure);
}

bool TypePredicates::isClass(const ScamData * data)
{
    return data->type == ScamData::Class;
}

bool TypePredicates::isInstance(const ScamData * data)
{
    return data->type == ScamData::Instance;
}

bool TypePredicates::isDict(const ScamData * data)
{
    return data->type == ScamData::Dict;
}
