#include "util/NumericConverter.hpp"

#include "expr/ExpressionFactory.hpp"
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
    , exact(true)
    , OK(true)
{
    convert();
}

ExprHandle NumericConverter::getValue() const
{
    return value;
}

const char * NumericConverter::getPos() const
{
    return pos;
}

void NumericConverter::convert()
{
    if ( scanSpecialValue() ) {
        return;
    }

    scanInitial();
    const int integerPart = scanInteger();
    const double fractionalPart = scanFraction();
    const int exponent = scanExponent();
    if ( ! StringTokenizer::isDelimiter(*pos) ) {
        OK = false;
        return;
    }

    finalizeExactness();
    makeResult(integerPart, fractionalPart, exponent);
}

bool NumericConverter::scanSpecialValue()
{
    if ( ! OK ) {
        return false;
    }

    if ( 0 == strncmp(pos, "+nan.0", 6) ) {
        value = ExpressionFactory::makeNaN();
    }
    else if ( 0 == strncmp(pos, "+inf.0", 6) ) {
        value = ExpressionFactory::makePosInf();
    }
    else if ( 0 == strncmp(pos, "-inf.0", 6) ) {
        value = ExpressionFactory::makeNegInf();
    }
    else {
        return false;
    }

    if ( StringTokenizer::isDelimiter(pos[6]) ) {
        pos += 6;
    }
    else {
        value = ExpressionFactory::makeNull();
        OK = false;
        return false;
    }

    return true;
}

void NumericConverter::scanInitial()
{
    if ( ! OK ) {
        return;
    }

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
            break;
        }

        pos += 2;
        if ( ! OK ) {
            return;
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
        OK = false;
        break;
    }
}

int NumericConverter::scanInteger()
{
    if ( ! OK ) {
        return 0;
    }

    const int sign = scanSign();
    const int value = scanUnsigned();
    return sign * value;
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

int NumericConverter::scanUnsigned()
{
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

    if ( count < 1 ) {
        OK = false;
    }

    return value;
}

double NumericConverter::scanFraction()
{
    if ( ! OK ) {
        return 0.0;
    }

    if ( ! scanRadixPoint() ) {
        return 0.0;
    }

    double value { 0.0 };
    double divisor { 1.0 };

    while ( *pos ) {
        int digit = convertDigit(*pos);
        if ( -1 == digit ) {
            break;
        }

        ++pos;
        divisor /= base;
        value = value + divisor * digit;
    }

    return value;
}

bool NumericConverter::scanRadixPoint()
{
    if ( '.' == *pos ) {
        if ( 10 == base ) {
            ++pos;
            exact = false;
            return true;
        }
        else {
            OK = false;
        }
    }
    return false;
}

int NumericConverter::scanExponent()
{
    if ( 'e' != tolower(*pos) ) {
        return 0;
    }

    if ( 10 != base ) {
        OK = false;
        return 0;
    }

    ++pos;
    exact = false;

    return scanInteger();
}

void NumericConverter::finalizeExactness()
{
    switch ( exactness ) {
    case ExactnessType::ET_EXACT :
        exact = true;
        break;
    case ExactnessType::ET_INEXACT :
        exact = false;
        break;
    default:
        break;
    }
}

void NumericConverter::makeResult(int integerPart,
                                  double fractionalPart,
                                  int exponent)
{
    if ( ! OK ) {
        return;
    }

    const double multiplier = makeMultiplier(exponent);
    if ( integerPart < 0 ) {
        fractionalPart *= -1;
    }
    const double result = ((double) integerPart + fractionalPart) * multiplier;

    if ( 0 == ::fmod(result, 1.0) ) {
        value = ExpressionFactory::makeInteger((int) result, exact);
    }
    else {
        value = ExpressionFactory::makeReal(result, exact);
    }
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

int NumericConverter::convertDigit(char digit)
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
