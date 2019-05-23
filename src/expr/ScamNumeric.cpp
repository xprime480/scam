#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "util/NumericUtils.hpp"

#include <cctype>
#include <cmath>
#include <sstream>

using namespace scam;
using namespace std;

ConstScamValue ScamNumeric::realPart(const ScamData * data)
{
    if ( ! TypePredicates::isNumeric(data) ) {
        stringstream s;
        s << "<" << writeValue(data) << "> is not numeric; has no real part";
        throw ScamException(s.str());
    }

    if ( TypePredicates::isPureComplex(data) ) {
        return REALPART(data);
    }

    return ExpressionFactory::makeNull(); // temporary hack!!
}

ConstScamValue ScamNumeric::imagPart(const ScamData * data)
{
    if ( ! TypePredicates::isNumeric(data) ) {
        stringstream s;
        s << "<" << writeValue(data)
          << "> is not numeric; has no imaginary part";
        throw ScamException(s.str());
    }

    if ( TypePredicates::isPureComplex(data) ) {
        return IMAGPART(data);
    }

    return ExpressionFactory::makeInteger(0, true);
}

ScamNumeric::ScamNumeric(ScamData::NaNType tag)
    : ScamExpr(ScamData::NaN, false)
{
    EXACT(this) = false;
}

ScamNumeric::ScamNumeric(ScamData::NegInfType tag)
    : ScamExpr(ScamData::NegInf, false)
{
    EXACT(this) = false;
}

ScamNumeric::ScamNumeric(ScamData::PosInfType tag)
    : ScamExpr(ScamData::PosInf, false)
{
    EXACT(this) = false;
}

ScamNumeric::ScamNumeric(ScamValue real, ScamValue imag, bool managed)
    : ScamExpr(ScamData::Complex, managed)
{
    EXACT(this) = EXACT(real) && EXACT(imag);

    if ( TypePredicates::isPureComplex(real) ||
         TypePredicates::isPureComplex(imag) ) {
        static string msg =
            "Cannot set either part a complex number to another complex number";
        throw ScamException(msg);
    }

    REALPART(this) = real;
    IMAGPART(this) = imag;
}

ScamNumeric::ScamNumeric(double value, bool exact, bool managed)
    : ScamExpr(ScamData::Real, managed)
{
    EXACT(this) = exact;
    REALVAL(this) = value;
}

ScamNumeric::ScamNumeric(int num, int den, bool exact, bool managed)
    : ScamExpr(ScamData::Rational, managed)
{
    EXACT(this) = exact;

    const int div = gcd(num, den);
    NUMPART(this) = num / div;
    DENPART(this) = den / div;
}

ScamNumeric::ScamNumeric(int value, bool exact, bool managed)
    : ScamExpr(ScamData::Integer, managed)
{
    EXACT(this) = exact;
    INTVAL(this) = value;
}

ScamNumeric * ScamNumeric::makeInstance(ScamData::NaNType tag)
{
    static ScamNumeric instance(tag);
    return &instance;
}

ScamNumeric * ScamNumeric::makeInstance(ScamData::NegInfType tag)
{
    static ScamNumeric instance(tag);
    return &instance;
}

ScamNumeric * ScamNumeric::makeInstance(ScamData::PosInfType tag)
{
    static ScamNumeric instance(tag);
    return &instance;
}

ScamNumeric *
ScamNumeric::makeInstance(ScamValue real, ScamValue imag, bool managed)
{
    return new ScamNumeric(real, imag, managed);
}

ScamNumeric *
ScamNumeric::makeInstance(double value, bool exact, bool managed)
{
    return new ScamNumeric(value, exact, managed);
}

ScamNumeric *
ScamNumeric::makeInstance(int num, int den, bool exact, bool managed)
{
    return new ScamNumeric(num, den, exact, managed);
}

ScamNumeric *
ScamNumeric::makeInstance(int value, bool exact, bool managed)
{
    return new ScamNumeric(value, exact, managed);
}

bool ScamNumeric::equals(ConstScamValue expr) const
{
    if ( ! TypePredicates::isNumeric(expr) ) {
        return false;
    }

    if ( TypePredicates::isNaN(this) || TypePredicates::isNaN(expr) ) {
        return TypePredicates::isNaN(this) && TypePredicates::isNaN(expr);
    }
    if ( TypePredicates::isNegInf(this) || TypePredicates::isNegInf(expr) ) {
        return TypePredicates::isNegInf(this) && TypePredicates::isNegInf(expr);
    }
    if ( TypePredicates::isPosInf(this) || TypePredicates::isPosInf(expr) ) {
        return TypePredicates::isPosInf(this) && TypePredicates::isPosInf(expr);
    }

    /* temporary hack!!! */
    ConstScamValue thisH = realPart(this);
    ConstScamValue thatH = realPart(expr);

    const double thisR = asDouble(TypePredicates::isNull(thisH) ? this : thisH);
    const double thatR = asDouble(TypePredicates::isNull(thatH) ? expr : thatH);

    if ( ::fabs(thisR- thatR) > 1e-9 ) {
        return false;
    }

    if ( TypePredicates::isPureComplex(this) ||
         TypePredicates::isPureComplex(expr) ) {
        const double thisI = asDouble(imagPart(this));
        const double thatI = asDouble(imagPart(expr));
        if ( ::fabs(thisI- thatI) > 1e-9 ) {
            return false;
        }
    }

    return true;
}
