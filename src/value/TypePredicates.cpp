#include "value/TypePredicates.hpp"

#include "ScamException.hpp"
#include "expr/SequenceOps.hpp"
#include "value/ScamData.hpp"
#include "value/ValueWriter.hpp"

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
    return data->type == ScamValueType::Nothing;
}

bool scam::isAnything(ScamValue data)
{
    return ! isNothing(data);
}

bool scam::isError(ScamValue data)
{
    return data->type == ScamValueType::Error;
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
    return data->type == ScamValueType::Boolean;
}

bool scam::isChar(ScamValue data)
{
    return data->type == ScamValueType::Character;
}

bool scam::isString(ScamValue data)
{
    return data->type == ScamValueType::String;
}

bool scam::isSymbol(ScamValue data)
{
    return data->type == ScamValueType::Symbol;
}

bool scam::isKeyword(ScamValue data)
{
    return data->type == ScamValueType::Keyword;
}

bool scam::isNumeric(ScamValue data)
{
    return ( isComplex(data) || isSpecialNumeric(data) );
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
    return ( isPureComplex(data) || isReal(data) );
}

bool scam::isPureComplex(ScamValue data)
{
    return data->type == ScamValueType::Complex;
}

bool scam::isReal(ScamValue data)
{
    return ( (data->type == ScamValueType::Real) ||
             isRational(data) ||
             isSpecialNumeric(data) );
}

bool scam::isRational(ScamValue data)
{
    return ( (data->type == ScamValueType::Rational) || isInteger(data) );
}

bool scam::isInteger(ScamValue data)
{
    return data->type == ScamValueType::Integer;
}

bool scam::isNaN(ScamValue data)
{
    return data->type == ScamValueType::NaN;
}

bool scam::isNegInf(ScamValue data)
{
    return data->type == ScamValueType::NegInf;
}

bool scam::isPosInf(ScamValue data)
{
    return data->type == ScamValueType::PosInf;
}

bool scam::isSpecialNumeric(ScamValue data)
{
    return ( isNaN(data) || isNegInf(data) || isPosInf(data) );
}

bool scam::isNull(ScamValue data)
{
    return data->type == ScamValueType::Null;
}

bool scam::isPair(ScamValue data)
{
    return data->type == ScamValueType::Pair;
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
    return data->type == ScamValueType::Vector;
}

bool scam::isByteVector(ScamValue data)
{
    return data->type == ScamValueType::ByteVector;
}

bool scam::isClosure(ScamValue data)
{
    return data->type == ScamValueType::Closure;
}

bool scam::isProcedure(ScamValue data)
{
    return ( isClosure(data) || isClass(data) || isInstance(data) );
}

bool scam::isClass(ScamValue data)
{
    return data->type == ScamValueType::Class;
}

bool scam::isInstance(ScamValue data)
{
    return data->type == ScamValueType::Instance;
}

bool scam::isDict(ScamValue data)
{
    return data->type == ScamValueType::Dict;
}

bool scam::isSpecialForm(ScamValue data)
{
    return data->type == ScamValueType::SpecialForm;
}

bool scam::isPrimitive(ScamValue data)
{
    return data->type == ScamValueType::Primitive;
}

bool scam::isApplicable(ScamValue data)
{
    return ( isDict(data) ||
             isProcedure(data) ||
             isPrimitive(data) ||
             isSpecialForm(data) ||
             isContinuation(data) );
}

bool scam::isContinuation(ScamValue data)
{
    return data->type == ScamValueType::Cont;
}

bool scam::isScamProcedure(ScamValue data)
{
    return isProcedure(data) || isApplicable(data);
}

bool scam::isPort(ScamValue data)
{
    return data->type == ScamValueType::Port;
}

bool scam::isEof(ScamValue data)
{
    return data->type == ScamValueType::Eof;
}

bool scam::isSyntax(ScamValue data)
{
    return data->type == ScamValueType::Syntax;
}

bool scam::isEnv(ScamValue data)
{
    return data->type == ScamValueType::ScamEnv;
}

bool scam::isForwarder(ScamValue data)
{
    return isEnv(data) && truth(data->hasMeta("forward-address"));
}

bool scam::isPlaceholder(ScamValue data)
{
    return data->type == ScamValueType::Placeholder;
}

bool scam::isMultiple(ScamValue data)
{
    return data->type == ScamValueType::Multiple;
}
