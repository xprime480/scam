#include "util/NumericConverter.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ExtendedNumeric.hpp"
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

ExprHandle NumericConverter::simplify(ExprHandle value)
{
    if ( ! value->isNumeric() ) {
        return value;
    }

    if ( value->isNaN() || value->isNegInf() || value->isPosInf() ) {
        return value;
    }

    if ( value->isInteger() ) {
        return value;
    }

    if ( value->isReal() ) {
        double v = value->toReal();
        if ( 0 == ::fmod(v, 1.0) ) {
            value = ExpressionFactory::makeInteger((int)v, value->isExact());
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
}

void NumericConverter::scanComplex()
{
    ExprHandle rv = scanReal();
    value = simplify(rv);
}

ExprHandle NumericConverter::scanReal()
{
    ExprHandle rv = scanInfNan();
    if ( ! rv->isNull() ) {
        return rv;
    }

    const char * original = pos;

    const int sign = scanSign();
    rv = scanUReal();

    if ( rv->isNull() ) {
        pos = original;
        return rv;
    }

    if ( sign < 0 && rv->isReal() ) {
        ExtendedNumeric lhs(ExpressionFactory::makeInteger(sign, true));
        ExtendedNumeric rhs(rv);
        ExtendedNumeric result = lhs * rhs;

        rv = result.get();
    }

    return rv;
}

ExprHandle NumericConverter::scanUReal()
{
    const char * original = pos;

    if ( 10 == base ) {
        ExprHandle rv = scanDecimal();
        if ( ! rv->isNull() ) {
            return rv;
        }
    }

    ExprHandle rv = scanUInteger();
    if ( rv->isNull() ) {
        pos = original;
    }

    return rv;
}

ExprHandle NumericConverter::scanDecimal()
{
    const char * original = pos;

    ExprHandle rv = scanDecimalCase3();
    if ( ! rv->isNull() ) {
        return rv;
    }

    rv = scanDecimalCase1();
    if ( ! rv->isNull() ) {
        return rv;
    }

    rv = scanDecimalCase2();
    if ( ! rv->isNull() ) {
        return rv;
    }

    pos = original;
    return rv;
}

ExprHandle NumericConverter::scanDecimalCase1()
{
    const char * original = pos;

    ExprHandle rv = scanUInteger();
    if ( rv->isNull() ) {
        pos = original;
        return rv;
    }

    ExprHandle suffix = scanSuffix();
    if ( ! suffix->isReal() ) {
        return rv;
    }

    ExtendedNumeric lhs(rv);
    ExtendedNumeric rhs(suffix);
    ExtendedNumeric result = lhs * rhs;

    return result.get();
}

ExprHandle NumericConverter::scanDecimalCase2()
{
    const char * original = pos;

    if ( ! scanRadixPoint() ) {
        return ExpressionFactory::makeNull();
    }

    ExprHandle rv = makeFraction(1);
    if ( rv->isNull() ) {
        pos = original;
        return rv;
    }

    ExprHandle suffix = scanSuffix();
    if ( suffix->isReal() ) {
        ExtendedNumeric lhs(rv);
        ExtendedNumeric rhs(suffix);
        ExtendedNumeric result = lhs * rhs;
        rv = result.get();
    }

    return rv;
}

ExprHandle NumericConverter::scanDecimalCase3()
{
    const char * original = pos;

    ExprHandle ip = scanUInteger();
    if ( ip->isNull() ) {
        pos = original;
        return ip;
    }

    if ( ! scanRadixPoint() ) {
        pos = original;
        return ExpressionFactory::makeNull();
    }

    ExprHandle fp = makeFraction(0);
    if ( fp->isNull() ) {
        pos = original;
        return fp;
    }

    ExtendedNumeric lhs(ip);
    ExtendedNumeric rhs(fp);
    ExtendedNumeric result = lhs + rhs;
    ExprHandle rv = result.get();

    ExprHandle xp = scanSuffix();
    if ( xp->isReal() ) {
        ExtendedNumeric lhs(rv);
        ExtendedNumeric rhs(xp);
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
        if ( value->isInteger() ) {
            double suffix = makeMultiplier(sign * value->toInteger());
            rv = makeRealWithExactness(suffix);
        }
    }

    if ( rv->isNull() ) {
        pos = original;
    }

    return rv;
}

int NumericConverter::scanSign()
{
    if ( '+' == *pos ) {
        ++pos;
    }
    else if ( '-' == *pos ) {
        ++pos;
        return -1;
    }
    return 1;
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

    double value { 0.0 };
    double divisor { 1.0 };
    unsigned count { 0 };

    while ( *pos ) {
        int digit = convertDigit(*pos);
        if ( -1 == digit ) {
            break;
        }

        ++count;
        ++pos;
        divisor /= base;
        value = value + divisor * digit;
    }

    if ( count < minCount ) {
        pos = original;
        return ExpressionFactory::makeNull();
    }

    return makeRealWithExactness(value);
}

bool NumericConverter::scanRadixPoint()
{
    if ( '.' == *pos ) {
        if ( 10 == base ) {
            ++pos;
            return true;
        }
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

ExprHandle NumericConverter::makeRealWithExactness(double value) const
{
    bool makeExact = exactness == ExactnessType::ET_EXACT;
    return simplify(ExpressionFactory::makeReal(value, makeExact));
}

ExprHandle NumericConverter::makeIntegerWithExactness(int value) const
{
    bool makeExact = ! (exactness == ExactnessType::ET_INEXACT);
    return ExpressionFactory::makeInteger(value, makeExact);
}
