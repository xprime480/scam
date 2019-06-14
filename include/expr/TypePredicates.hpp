#if ! defined(TYPEPREDICATES_HPP)
#define TYPEPREDICATES_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern bool isImmutable(ScamValue data);

    extern bool isNothing(ScamValue data);
    extern bool isAnything(ScamValue data);
    extern bool isError(ScamValue data);
    extern bool truth(ScamValue data);

    extern bool isBoolean(ScamValue data);
    extern bool isChar(ScamValue data);
    extern bool isString(ScamValue data);
    extern bool isSymbol(ScamValue data);
    extern bool isKeyword(ScamValue data);

    extern bool isNumeric(ScamValue data);
    extern bool isExact(ScamValue data);
    extern bool isComplex(ScamValue data);
    extern bool isPureComplex(ScamValue data);
    extern bool isReal(ScamValue data);
    extern bool isRational(ScamValue data);
    extern bool isInteger(ScamValue data);

    extern bool isNaN(ScamValue data);
    extern bool isNegInf(ScamValue data);
    extern bool isPosInf(ScamValue data);
    extern bool isSpecialNumeric(ScamValue data);

    extern bool isNull(ScamValue data);
    extern bool isPair(ScamValue data);
    extern bool isList(ScamValue data);

    extern bool isVector(ScamValue data);
    extern bool isByteVector(ScamValue data);
    extern bool isDict(ScamValue data);

    extern bool isClosure(ScamValue data);
    extern bool isProcedure(ScamValue data);
    extern bool isClass(ScamValue data);
    extern bool isInstance(ScamValue data);
    extern bool isSpecialForm(ScamValue data);
    extern bool isPrimitive(ScamValue data);
    extern bool isApplicable(ScamValue data);

    extern bool isContinuation(ScamValue data);

    extern bool isPort(ScamValue data);
}

#endif
