#include "expr/TypePredicates.hpp"

#include "expr/ScamData.hpp"
#include "expr/ScamNumeric.hpp"

using namespace scam;

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
    return ScamNumeric::isNumeric(data);
}

bool TypePredicates::isExact(const ScamData * data)
{
    return ScamNumeric::isExact(data);
}

bool TypePredicates::isComplex(const ScamData * data)
{
    return ScamNumeric::isComplex(data);
}

bool TypePredicates::isReal(const ScamData * data)
{
    return ScamNumeric::isReal(data);
}

bool TypePredicates::isRational(const ScamData * data)
{
    return ScamNumeric::isRational(data);
}

bool TypePredicates::isInteger(const ScamData * data)
{
    return ScamNumeric::isInteger(data);
}

bool TypePredicates::isNaN(const ScamData * data)
{
    return ScamNumeric::isNaN(data);
}

bool TypePredicates::isNegInf(const ScamData * data)
{
    return ScamNumeric::isNegInf(data);
}

bool TypePredicates::isPosInf(const ScamData * data)
{
    return ScamNumeric::isPosInf(data);
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
        return isList(CDR(data));
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

