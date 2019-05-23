#include "util/NumericConverter.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "expr/TypePredicates.hpp"
#include "input/StringTokenizer.hpp"

#include <cmath>
#include <cstring>

using namespace scam;
using namespace std;

NumericConverter::NumericConverter(const char * pos)
    : pos(pos)
    , value(ExpressionFactory::makeNull())
    , base(10)
    , exactness(ExactnessType::ET_CONTEXT)
{
    scanNum();
}

ExprHandle includeSign(int sign, ExprHandle expr)
{
    if ( sign < 0 ) {
        ExtendedNumeric lhs(ExpressionFactory::makeInteger(sign, true));
        ExtendedNumeric rhs(expr);
        ExtendedNumeric result = lhs * rhs;
        expr = result.get();
    }
    return expr;
}

ExprHandle NumericConverter::simplify(ExprHandle value)
{
    if ( ! TypePredicates::isNumeric(value) ) {
        return value;
    }

    if ( TypePredicates::isNaN(value) ||
         TypePredicates::isNegInf(value) ||
         TypePredicates::isPosInf(value) ) {
        return value;
    }

    if ( TypePredicates::isComplex(value) &&
         ! TypePredicates::isReal(value) ) {
        ExprHandle imag = simplify(const_cast<ExprHandle>(value->imagPart()));
        if ( imag && TypePredicates::isInteger(imag) &&
             0 == imag->asInteger() ) {
            value = const_cast<ExprHandle>(value->realPart());
        }
    }

    if ( TypePredicates::isReal(value) &&
         ! TypePredicates::isRational(value) ) {
        double v = value->asDouble();
        double frac = ::fmod(v, 1.0);
        if ( 0.0 == frac ) {
            const bool ex = TypePredicates::isExact(value);
            value = ExpressionFactory::makeRational((int)v, 1, ex);
        }
        else {
            return value;
        }
    }

    if ( TypePredicates::isRational(value) &&
         ! TypePredicates::isInteger(value) ) {
        const pair<int, int> v = value->asRational();
        if ( 1 == v.second ) {
            const bool ex = TypePredicates::isExact(value);
            value = ExpressionFactory::makeInteger(v.first, ex);
        }
    }

    return value;
}

ExprHandle NumericConverter::getValue() const
{
    return value;
}

const char * NumericConverter::getPos() const
{
    return pos;
}

void NumericConverter::scanNum()
{
    scanPrefix();
    scanComplex();

    if ( ! StringTokenizer::isDelimiter(*pos) ) {
        value = ExpressionFactory::makeNull();
    }
}

void NumericConverter::scanComplex()
{
    ExprHandle rv = ExpressionFactory::makeNull();
    const char * original = pos;

    ExprHandle real = scanInfNan();
    ExprHandle imag = ExpressionFactory::makeNull();

    if ( TypePredicates::isNull(real) ) { // not infnan
        int sign = scanSign(false);
        if ( 0 == sign ) {
            rv = imag;
        }
        else {
            real = makeIntegerWithExactness(0);
            imag = scanUReal();
            if ( TypePredicates::isNull(imag) ) {
                imag = makeIntegerWithExactness(1);
            }
            if ( 'i' == tolower(*pos) ) {
                ++pos;
                imag = includeSign(sign, imag);
                rv = ExpressionFactory::makeComplex(real, imag);
            }
        }
    }
    else {                      // infnan
        imag = scanReal();
        if ( 'i' == tolower(*pos) ) {
            ++pos;
            if ( TypePredicates::isNull(imag) ) {
                imag = real;
                real = makeIntegerWithExactness(0);
            }
            rv = ExpressionFactory::makeComplex(real, imag);
        }
        else {
            rv = real;
        }
    }

    if ( TypePredicates::isNull(rv) ) { // not infnan and not pure imaginary
        // try again from the start for real [+/- imag]
        pos = original;
        real = scanReal();
        if ( TypePredicates::isNull(real) ) {
            rv = real;          // not any kind of number
        }
        else {
            original = pos;     // save in case rest does not compute
            int sign = 1;
            imag = scanInfNan();
            if ( TypePredicates::isNull(imag) ) {
                sign = scanSign();
                if ( 0 == sign ) {
                    imag = makeIntegerWithExactness(0);
                }
                else {
                    imag = scanUReal();
                    if ( TypePredicates::isNull(imag) ) {
                        imag = makeIntegerWithExactness(1);
                    }
                }
            }

            if ( 'i' == tolower(*pos) ) {
                ++pos;
                imag = includeSign(sign, imag);
            }
            else {
                pos = original;
                rv = real;
            }

            if ( TypePredicates::isNull(rv) ) {
                rv = ExpressionFactory::makeComplex(real, imag);
            }
        }
    }

    if ( TypePredicates::isReal(rv) ) {
        if ( '@' == *pos ) {
            ++pos;
            imag = scanReal();
            if ( TypePredicates::isNull(imag) ) {
                --pos;
            }
            else {
                rv = makeComplexPolar(rv, imag);
            }
        }
    }

    value = simplify(rv);
}

ExprHandle NumericConverter::scanReal()
{
    ExprHandle rv = scanInfNan();
    if ( ! TypePredicates::isNull(rv) ) {
        return rv;
    }

    const char * original = pos;

    const int sign = scanSign();
    rv = scanUReal();

    if ( TypePredicates::isNull(rv) ) {
        pos = original;
        return rv;
    }

    rv = includeSign(sign, rv);
    return rv;
}

ExprHandle NumericConverter::scanUReal()
{
    const char * original = pos;

    ExprHandle rv10 { ExpressionFactory::makeNull() };
    const char * pos10 = pos;
    if ( 10 == base ) {
        rv10 = scanDecimal();
        if ( ! TypePredicates::isNull(rv10) ) {
            pos10 = pos;
        }
    }

    pos = original;

    ExprHandle rvN = scanUInteger();
    if ( TypePredicates::isNull(rvN) ) {
        pos = pos10;
        return rv10;
    }

    const char * posN = pos;
    original = pos;
    if ( '/' != *pos ) {
        if ( posN > pos10 ) {
            pos = posN;
            return rvN;
        }
        else {
            pos = pos10;
            return rv10;
        }
    }

    ++pos;

    ExprHandle rvD = scanUInteger();
    if ( TypePredicates::isNull(rvD) ) {
        pos = original;
        return ( posN > pos10 ) ? rvN : rv10;
    }

    ExprHandle rv = makeRationalWithExactness(rvN->asInteger(),
                                              rvD->asInteger());
    return rv;
}

ExprHandle NumericConverter::scanDecimal()
{
    const char * original = pos;
    ExprHandle rv { ExpressionFactory::makeNull() };

    if ( scanRadixPoint() ) {
        rv = makeFraction(1);
        if ( TypePredicates::isNull(rv) ) {
            pos = original;
            return rv;
        }
    }
    else {
        rv = scanUInteger();
        if ( TypePredicates::isNull(rv) ) {
            pos = original;
            return rv;
        }

        if ( scanRadixPoint() ) {
            ExprHandle fp = makeFraction(0);
            if ( ! TypePredicates::isNull(rv) ) {
                ExtendedNumeric lhs(rv);
                ExtendedNumeric rhs(fp);
                ExtendedNumeric result = lhs + rhs;
                rv = result.get();
            }
        }
    }

    ExprHandle suffix = scanSuffix();
    if ( TypePredicates::isReal(suffix) ) {
        ExtendedNumeric lhs(rv);
        ExtendedNumeric rhs(suffix);
        ExtendedNumeric result = lhs * rhs;
        rv = result.get();
    }

    return rv;
}

ExprHandle NumericConverter::scanUInteger()
{
    const char * original = pos;

    size_t count { 0 };
    int value { 0 };

    while ( *pos ) {
        int digit = convertDigit(*pos);
        if ( -1 == digit ) {
            break;
        }

        ++pos;
        ++count;
        value = value * base + digit;
    }

    if ( 0 == count ) {
        pos = original;
        return ExpressionFactory::makeNull();
    }

    return makeIntegerWithExactness(value);
}

void NumericConverter::scanPrefix()
{
    while ( '#' == *pos ) {
        switch ( pos[1] ) {
        case 'e': case 'E':
        case 'i': case 'I':
            exactnessSeen(pos[1]);
            break;

        case 'b': case 'B':
        case 'o': case 'O':
        case 'd': case 'D':
        case 'x': case 'X':
            baseSeen(pos[1]);
            break;

        default:
            return;
            break;
        }

        pos += 2;
    }
}

ExprHandle NumericConverter::scanInfNan()
{
    ExprHandle rv { ExpressionFactory::makeNull() };

    if ( 0 == strncmp(pos, "+nan.0", 6) || 0 == strncmp(pos, "-nan.0", 6) ) {
        rv = ExpressionFactory::makeNaN();
    }
    else if ( 0 == strncmp(pos, "+inf.0", 6) ) {
        rv = ExpressionFactory::makePosInf();
    }
    else if ( 0 == strncmp(pos, "-inf.0", 6) ) {
        rv = ExpressionFactory::makeNegInf();
    }
    else {
        return rv;
    }

    pos += 6;
    return rv;
}

ExprHandle NumericConverter::scanSuffix()
{
    const char * original = pos;
    ExprHandle rv { ExpressionFactory::makeNull() };

    if ( 'e' == tolower(*pos) ) {
        ++pos;
        int sign = scanSign();
        ExprHandle value = scanUInteger();
        if ( TypePredicates::isInteger(value) ) {
            double suffix = makeMultiplier(sign * value->asInteger());
            rv = makeRealWithExactness(suffix);
        }
    }

    if ( TypePredicates::isNull(rv) ) {
        pos = original;
    }

    return rv;
}

int NumericConverter::scanSign(bool optional)
{
    if ( '+' == *pos ) {
        ++pos;
        return 1;
    }
    else if ( '-' == *pos ) {
        ++pos;
        return -1;
    }
    else {
        if ( optional ) {
            return 1;
        }
        else {
            return 0;
        }
    }
}

void NumericConverter::exactnessSeen(char x)
{
    if ( 'e' == tolower(x) ) {
        exactness = ExactnessType::ET_EXACT;
    }
    else {
        exactness = ExactnessType::ET_INEXACT;
    }
}

void NumericConverter::baseSeen(char x)
{
    switch ( tolower(x) ) {
    case 'b':
        base = 2;
        break;
    case 'o':
        base = 8;
        break;
    case 'd':
        base = 10;
        break;
    case 'x':
        base = 16;
        break;
    default:
        break;
    }
}

ExprHandle NumericConverter::makeFraction(unsigned minCount)
{
    const char * original = pos;
    ExprHandle rv = scanUInteger();

    unsigned count = pos - original;
    if ( count < minCount ) {
        pos = original;
        return ExpressionFactory::makeNull();
    }

    bool makeExact = exactness == ExactnessType::ET_EXACT;

    if ( TypePredicates::isNull(rv) ) {
        return ExpressionFactory::makeInteger(0, makeExact);
    }


    if ( count <= 6 ) {
        int value = rv->asInteger();
        double multiplier = makeMultiplier(count);
        return ExpressionFactory::makeRational(value, multiplier, makeExact);
    }
    else {

        pos = original;
        double divisor = 1.0;
        double value = 0.0;

        while ( isdigit(*pos) ) {
            divisor /= base;
            value += divisor * convertDigit(*pos);
            ++pos;
        }

        return makeRealWithExactness(value);
    }
}

bool NumericConverter::scanRadixPoint()
{
    if ( '.' == *pos ) {
        ++pos;
        return true;
    }

    return false;
}

double NumericConverter::makeMultiplier(int exponent) const
{
    if ( 0 == exponent ) {
        return 1.0;
    }
    if ( 1 == exponent ) {
        return (double) base;
    }
    if ( exponent < 0 ) {
        return 1.0 / makeMultiplier(-1 * exponent);
    }

    double temp = makeMultiplier(exponent / 2);
    temp = temp * temp;
    if ( exponent & 1 ) {
        temp = temp * base;
    }

    return temp;
}

int NumericConverter::convertDigit(char digit) const
{
    if ( ! isalnum(digit) ) {
        return -1;
    }

    switch ( base ) {
    case 2: case 8: case 10:
        {
            int rv = digit - '0';
            return ( rv >= 0 && rv <= (base - 1) ) ? rv : -1;
        }
        break;

    case 16:
        {
            if ( isdigit(digit) ) {
                return digit - '0';
            }
            int rv = 10 + (tolower(digit) - 'a');
            return ( rv >= 10 && rv <= 15 ) ? rv : -1;
        }
        break;

    default:
        return -1;
        break;
    }

    return -1;
}

ExprHandle
NumericConverter::makeComplexPolar(ExprHandle r, ExprHandle theta) const
{
    ExprHandle nan = ExpressionFactory::makeNaN();
    ExprHandle real;
    ExprHandle imag;

    if ( TypePredicates::isNaN(r) ||
         TypePredicates::isNegInf(r) ||
         TypePredicates::isPosInf(r) ) {
        real = imag = nan;
    }
    else if ( 0.0 == r->asDouble() ) {
        real = imag = r;
    }
    else if ( TypePredicates::isNaN(theta) ||
              TypePredicates::isNegInf(theta) ||
              TypePredicates::isPosInf(theta) ) {
        real = imag = nan;
    }
    else {
        const double radius = r->asDouble();
        const double cosine = ::cos(theta->asDouble());
        const double sine   = ::sin(theta->asDouble());

        const double x = radius * cosine;
        const double y = radius * sine;
        real = ExpressionFactory::makeReal(x, false);
        imag = ExpressionFactory::makeReal(y, false);
    }

    ExprHandle rv = ExpressionFactory::makeComplex(real, imag);
    return rv;
}

ExprHandle NumericConverter::makeRealWithExactness(double value) const
{
    bool makeExact = exactness == ExactnessType::ET_EXACT;
    return simplify(ExpressionFactory::makeReal(value, makeExact));
}

ExprHandle NumericConverter::makeRationalWithExactness(int num, int den) const
{
    bool makeExact = ! (exactness == ExactnessType::ET_INEXACT);
    return ExpressionFactory::makeRational(num, den, makeExact);
}

ExprHandle NumericConverter::makeIntegerWithExactness(int value) const
{
    bool makeExact = ! (exactness == ExactnessType::ET_INEXACT);
    return ExpressionFactory::makeInteger(value, makeExact);
}
