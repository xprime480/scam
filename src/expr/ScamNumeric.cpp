#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/NumericUtils.hpp"

#include <cctype>
#include <cmath>
#include <sstream>

#include "util/DebugTrace.hpp"

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

    bool isPureComplex(ConstExprHandle expr)
    {
        return expr->isComplex() && ! expr->isReal();
    }
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

ScamNumeric::ScamNumeric(ExprHandle real, ExprHandle imag, bool managed)
    : ScamExpr(managed)
    , exact(real->isExact() && imag->isExact())
    , type(ScamNumericComplex)
{
    if ( isPureComplex(real) || isPureComplex(imag) ) {
        static string msg =
            "Cannot set either part a complex number to another complex number";
        throw ScamException(msg);
    }

    this->value.complexValue.real = real;
    this->value.complexValue.imag = imag;
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
ScamNumeric::makeInstance(ExprHandle real, ExprHandle imag, bool managed)
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

void ScamNumeric::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();

        if ( isPureComplex(this) ) {
            value.complexValue.real->mark();
            value.complexValue.imag->mark();
        }
    }
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
    else if ( isComplex() ) {
        //
        // The complexity is so that the output is in the simplest
        // form that will be read by the scanner as the same value.
        // For example, An imaginary part of "-1i" is equivalent to
        // "-i", so the latter is used for the representation.  The
        // real pa
        //
        ExprHandle r { value.complexValue.real };
        ExprHandle i { value.complexValue.imag };

        if ( ! r->isInteger() || 0 != r->asInteger() ) {
            s << value.complexValue.real->toString();
        }

        const string irepr = i->toString();
        if ( irepr == "0" ) {
            // nothing
        }
        if ( irepr == "1" ) {
            s << "+";
        }
        else if ( irepr == "-1" ) {
            s << "-";
        }
        else {
            const char lead = *irepr.c_str();
            if ( ('+' != lead) && ('-' != lead) ) {
                s << "+";
            }
            s << irepr;
        }
         s << "i";
    }
    else {
        s << "@obj<" << this << ">";
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

double ScamNumeric::asDouble() const
{
    if ( ! isReal() || isNaN() || isNegInf() || isPosInf() ) {
        stringstream s;
        s << "Cannot convert <" << this->toString() << "> to real";
        throw ScamException(s.str());
    }

    if ( isInteger() ) {
        return (double) value.intValue;
    }
    else if ( isRational() ) {
        return ((double) value.rationalValue.num /
                (double) value.rationalValue.den );
    }
    else if ( isNaN() || isNegInf() || isPosInf() ) {
        // drop through to error case;
    }
    else if ( isReal() ) {
        return value.realValue;
    }

    stringstream s;
    s << "Cannot convert <" << this->toString() << "> to double";
    throw ScamException(s.str());

    return 0.0;
}

std::pair<int, int> ScamNumeric::asRational() const
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

int ScamNumeric::asInteger() const
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

    const double thisR = this->realPart()->asDouble();
    const double thatR = that->realPart()->asDouble();
    if ( ::fabs(thisR- thatR) > 1e-9 ) {
        return false;
    }

    if ( isPureComplex(this) || isPureComplex(expr) ) {
        const double thisI = this->imagPart()->asDouble();
        const double thatI = that->imagPart()->asDouble();
        if ( ::fabs(thisI- thatI) > 1e-9 ) {
            return false;
        }
    }

    return true;
}

bool ScamNumeric::isExact() const
{
    return exact;
}

ConstExprHandle ScamNumeric::realPart() const
{
    if ( isPureComplex(this) ) {
        return value.complexValue.real;
    }
    return this;
}

ConstExprHandle ScamNumeric::imagPart() const
{
    if ( isPureComplex(this) ) {
        return value.complexValue.imag;
    }

    return ExpressionFactory::makeInteger(0, true);
}

