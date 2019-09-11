#include "expr/TypePredicates.hpp"

#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern bool isListFast(ScamValue data);
}

bool scam::isImmutable(ScamValue data)
{
    return data->isImmutable();
}

bool scam::isNothing(ScamValue data)
{
    return data->type == ScamData::Nothing;
}

bool scam::isAnything(ScamValue data)
{
    return ! isNothing(data);
}

bool scam::isError(ScamValue data)
{
    return data->type == ScamData::Error;
}

bool scam::isUnhandledError(ScamValue data)
{
    return isError(data) && ! data->errorHandled();
}

bool scam::truth(ScamValue data)
{
    if ( isNothing(data) ) {
        return false;
    }
    if ( ! isBoolean(data) ) {
        return true;
    }

    return data->boolValue();
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

    return data->exactFlag();
}

bool scam::isInexact(ScamValue data)
{
    return ! isExact(data);
}

bool scam::isComplex(ScamValue data)
{
    return 0 != (data->type & ScamData::ComplexTypes);
}

bool scam::isPureComplex(ScamValue data)
{
    return data->type == ScamData::Complex;
}

bool scam::isReal(ScamValue data)
{
    return 0 != (data->type & ScamData::RealTypes);
}

bool scam::isRational(ScamValue data)
{
    return 0 != (data->type & ScamData::RationalTypes);
}

bool scam::isInteger(ScamValue data)
{
    return data->type == ScamData::Integer;
}

bool scam::isNaN(ScamValue data)
{
    return data->type == ScamData::NaN;
}

bool scam::isNegInf(ScamValue data)
{
    return data->type == ScamData::NegInf;
}

bool scam::isPosInf(ScamValue data)
{
    return data->type == ScamData::PosInf;
}

bool scam::isSpecialNumeric(ScamValue data)
{
    return 0 != (data->type & ScamData::SpecialNumeric);
}

bool scam::isNull(ScamValue data)
{
    return data->type == ScamData::Null;
}

bool scam::isPair(ScamValue data)
{
    return data->type == ScamData::Pair;
}

bool scam::isList(ScamValue data)
{
    const auto shared = detectSharedStructure(data, false);
    if ( ! shared.empty() ) {
        return false;
    }

    return isListFast(data);
}

namespace
{
    bool isListFast(ScamValue data)
    {
        if ( isNull(data) ) {
            return true;
        }

        if ( isPair(data) ) {
            ScamValue cdr = data->cdrValue();
            return isList(cdr);
        }

        return false;
    }
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

bool scam::isScamProcedure(ScamValue data)
{
    return isProcedure(data) || isApplicable(data);
}

bool scam::isPort(ScamValue data)
{
    return data->type == ScamData::Port;
}

bool scam::isEof(ScamValue data)
{
    return data->type == ScamData::Eof;
}

bool scam::isSyntax(ScamValue data)
{
    return data->type == ScamData::Syntax;
}

bool scam::isEnv(ScamValue data)
{
    return data->type == ScamData::ScamEnv;
}

bool scam::isForwarder(ScamValue data)
{
    return isEnv(data) && truth(data->hasMeta("forward-address"));
}

bool scam::isPlaceholder(ScamValue data)
{
    return data->type == ScamData::Placeholder;
}
