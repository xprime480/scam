#include "util/NumericConverter.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamNumeric.hpp"

using namespace scam;
using namespace std;

NumericConverter::NumericConverter(const char * pos)
    : pos(pos)
    , value(ExpressionFactory::makeNull())
    , base(10)
    , exactness(ExactnessType::ET_CONTEXT)
    , negative(false)
    , exact(true)
    , type(NumericType::NT_INTEGER)
    , integerPart(0.0)
    , fractionalPart(0.0)
{
    convert();
}

ExprHandle NumericConverter::getValue() const
{
    return value;
}

void NumericConverter::convert()
{
    scanInitial();

    if ( '.' == *pos ) {
        return;
    }

    if ( ! scanIntegerPart() ) {
        return;
    }
    if ( ! scanFractionalPart() ) {
        return;
    }

    finalizeExactness();
    makeResult();
}

void NumericConverter::scanInitial()
{
    while ( '#' == *pos ) {
        switch ( pos[1] ) {
        case 'e': case 'E':
            exactness = ExactnessType::ET_EXACT;
            break;
        case 'i': case 'I':
            exactness = ExactnessType::ET_INEXACT;
            break;
        case 'b': case 'B':
            base = 2;
            break;
        case 'o': case 'O':
            base = 8;
            break;
        case 'd': case 'D':
            base = 10;
            break;
        case 'x': case 'X':
            base = 16;
            break;
        default:
            break;
        }
        pos += 2;
    }

    if ( '+' == *pos ) {
        ++pos;
    }
    else if ( '-' == *pos ) {
        ++pos;
        negative = true;
   }
}

bool NumericConverter::scanIntegerPart()
{
    while ( *pos ) {
        if ( '.' == *pos ) {
            ++pos;
            exact = false;
            break;
        }
        int digit = convertDigit(*pos);
        if ( -1 == digit ) {
            return false;
        }
        integerPart = integerPart * base + digit;
        ++pos;
    }

    return true;
}

bool NumericConverter::scanFractionalPart()
{
    double divisor = 1.0 / base;
    while ( *pos ) {
        int digit = convertDigit(*pos);
        if ( -1 == digit ) {
            return false;
        }
        if ( '0' != *pos ) {
            type = NumericType::NT_REAL;
        }
        fractionalPart = fractionalPart + d * digit;
        ++pos;
        divisor /= base;
    }

    return true;
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

void NumericConverter::makeResult()
{
    if ( NumericType::NT_INTEGER == type ) {
        int rv = (negative ? -1 : 1) * (int)integerPart;
        value = ExpressionFactory::makeInteger(rv, exact);
        return;
    }

    double t = integerPart + fractionalPart;
    double rv = negative ? -t : t;
    value = ExpressionFactory::makeReal(rv, exact);
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
