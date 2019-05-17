#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/NumericUtils.hpp"

#include <cctype>
#include <cmath>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    constexpr unsigned long ScamNumericComplexBit  { 1 << 0 };
    constexpr unsigned long ScamNumericRealBit     { 1 << 1 };
    constexpr unsigned long ScamNumericRationalBit { 1 << 2 };
    constexpr unsigned long ScamNumericIntegerBit  { 1 << 3 };

    constexpr unsigned long ScamNumericNaNBit      { 1 << 4 };
    constexpr unsigned long ScamNumericNegInfBit   { 1 << 5 };
    constexpr unsigned long ScamNumericPosInfBit   { 1 << 6 };

    constexpr unsigned long ScamNumericComplex =
        ScamNumericComplexBit;

    constexpr unsigned long ScamNumericReal =
        ScamNumericComplex | ScamNumericRealBit;

    constexpr unsigned long ScamNumericRational =
        ScamNumericReal | ScamNumericRationalBit;

    constexpr unsigned long ScamNumericInteger =
        ScamNumericRational | ScamNumericIntegerBit;

    constexpr unsigned long ScamNumericNaN =
        ScamNumericReal | ScamNumericNaNBit;

    constexpr unsigned long ScamNumericNegInf =
        ScamNumericReal | ScamNumericNegInfBit;

    constexpr unsigned long ScamNumericPosInf =
        ScamNumericReal | ScamNumericPosInfBit;
}

ScamNumeric::ScamNumeric(NaNType tag)
    : ScamExpr(false)
    , exact(false)
    , type(ScamNumericNaN)
{
}

ScamNumeric::ScamNumeric(NegInfType tag)
    : ScamExpr(false)
    , exact(false)
    , type(ScamNumericNegInf)
{
}

ScamNumeric::ScamNumeric(PosInfType tag)
    : ScamExpr(false)
    , exact(false)
    , type(ScamNumericPosInf)
{
}

ScamNumeric::ScamNumeric(double value, bool exact, bool managed)
    : ScamExpr(managed)
    , exact(exact)
    , type(ScamNumericReal)
{
    this->value.realValue = value;
}

ScamNumeric::ScamNumeric(int num, int den, bool exact, bool managed)
    : ScamExpr(managed)
    , exact(exact)
    , type(ScamNumericRational)
{
    const int div = gcd(num, den);
    value.rationalValue.num = num / div;
    value.rationalValue.den = den / div;
}

ScamNumeric::ScamNumeric(int value, bool exact, bool managed)
    : ScamExpr(managed)
    , exact(exact)
    , type(ScamNumericInteger)
{
    this->value.intValue = value;
}

ScamNumeric::ScamNumeric(bool exact, bool managed)
    : ScamExpr(managed)
    , exact(exact)
    , type(0)
{
}

ScamNumeric * ScamNumeric::makeInstance(NaNType tag)
{
    static ScamNumeric instance(tag);
    return &instance;
}

ScamNumeric * ScamNumeric::makeInstance(NegInfType tag)
{
    static ScamNumeric instance(tag);
    return &instance;
}

ScamNumeric * ScamNumeric::makeInstance(PosInfType tag)
{
    static ScamNumeric instance(tag);
    return &instance;
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

string ScamNumeric::toString() const
{
    stringstream s;

    if ( isNaN() ) {
        s << "+nan.0";
    }
    else if ( isNegInf() ) {
        s << "-inf.0";
    }
    else if ( isPosInf() ) {
        s << "+inf.0";
    }
    else if ( isInteger() ) {
        s << value.intValue;
    }
    else if ( isRational() ) {
        s << value.rationalValue.num << "/" << value.rationalValue.den;
    }
    else if ( isReal() ) {
        s << value.realValue;
    }

    return s.str();
}

bool ScamNumeric::isNumeric() const
{
    return true;
}

bool ScamNumeric::isComplex() const
{
    return ScamNumericComplexBit == (type & ScamNumericComplexBit);
}

bool ScamNumeric::isReal() const
{
    return ScamNumericRealBit == (type & ScamNumericRealBit);
}

bool ScamNumeric::isRational() const
{
    return ScamNumericRationalBit == (type & ScamNumericRationalBit);
}

bool ScamNumeric::isInteger() const
{
    return ScamNumericIntegerBit == (type & ScamNumericIntegerBit);
}

bool ScamNumeric::isNaN() const
{
    return ScamNumericNaNBit == (type & ScamNumericNaNBit);
}

bool ScamNumeric::isNegInf() const
{
    return ScamNumericNegInfBit == (type & ScamNumericNegInfBit);
}

bool ScamNumeric::isPosInf() const
{
    return ScamNumericPosInfBit == (type & ScamNumericPosInfBit);
}

double ScamNumeric::toReal() const
{
    if ( ! isReal() || isNaN() || isNegInf() || isPosInf() ) {
        stringstream s;
        s << "Cannot convert <" << this->toString() << "> to real";
        throw ScamException(s.str());
    }

    return realPart();
}

std::pair<int, int> ScamNumeric::toRational() const
{
    int num { 0 };
    int den { 1 };

    if ( isInteger() ) {
        num = value.intValue;
    }
    else if ( isRational() ) {
        num = value.rationalValue.num;
        den = value.rationalValue.den;
    }
    else {
        stringstream s;
        s << "Cannot convert <" << this->toString() << "> to rational";
        throw ScamException(s.str());
    }

    return make_pair<int,int>(move(num), move(den));
}

int ScamNumeric::toInteger() const
{
    if ( ! isInteger() ) {
        stringstream s;
        s << "Cannot convert <" << this->toString() << "> to integer";
        throw ScamException(s.str());
    }

    return value.intValue;
}

bool ScamNumeric::equals(ConstExprHandle expr) const
{
    if ( ! expr->isNumeric() ) {
        return false;
    }

    if ( isNaN() || expr->isNaN() ) {
        return isNaN() && expr->isNaN();
    }
    if ( isNegInf() || expr->isNegInf() ) {
        return isNegInf() && expr->isNegInf();
    }
    if ( isPosInf() || expr->isPosInf() ) {
        return isPosInf() && expr->isPosInf();
    }

    const ScamNumeric * that = dynamic_cast<const ScamNumeric *>(expr);

    const bool rv = (::fabs(this->realPart() - that->realPart()) < 1e-9 &&
                     ::fabs(this->imagPart() - that->imagPart()) < 1e-9 );
    return rv;
}

bool ScamNumeric::isExact() const
{
    return exact;
}

double ScamNumeric::realPart() const
{
    if ( isInteger() ) {
        return (double) value.intValue;
    }
    else if ( isRational() ) {
        return ((double) value.rationalValue.num /
                (double) value.rationalValue.den );
    }
    else if ( ! isNaN() && ! isNegInf() && ! isPosInf() ) {
        return value.realValue;
    }

    stringstream s;
    s << "Don't know how to take the real part of <" << this->toString() << ">";
    throw ScamException(s.str());

    return 0.0;
}

double ScamNumeric::imagPart() const
{
    return 0.0;
}

