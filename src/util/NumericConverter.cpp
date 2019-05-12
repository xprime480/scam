#include "util/NumericConverter.hpp"

#include "expr/ExpressionFactory.hpp"
#include "input/StringTokenizer.hpp"

#include <cstring>

using namespace scam;
using namespace std;

NumericConverter::NumericConverter(const char * pos)
    : pos(pos)
    , value(ExpressionFactory::makeNull())
    , base(10)
    , exactness(ExactnessType::ET_CONTEXT)
    , negative(false)
    , exact(true)
    , integerPart(0.0)
    , fractionalPart(0.0)
    , divisor(1.0)
    , state(ScanState::SS_BEGIN)
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
    if ( ScanState::SS_ERROR == state ) {
        return;
    }

    while ( ! StringTokenizer::isDelimiter(*pos) ) {
        const char c = *pos;
        handleChar(c);
        ++pos;
        if ( ScanState::SS_ERROR == state ) {
            return;
        }
    }

    finalizeExactness();
    makeResult();
}

bool NumericConverter::scanSpecialValue()
{
    if ( ScanState::SS_BEGIN != state ) {
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
        state = ScanState::SS_DONE;
    }
    else {
        value = ExpressionFactory::makeNull();
        state = ScanState::SS_ERROR;
    }

    return true;
}

void NumericConverter::scanInitial()
{
    if ( ScanState::SS_BEGIN != state ) {
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
        if ( ScanState::SS_ERROR == state ) {
            return;
        }
    }

    if ( '+' == *pos ) {
        state = ScanState::SS_SIGN;
        ++pos;
    }
    else if ( '-' == *pos ) {
        state = ScanState::SS_SIGN;
        ++pos;
        negative = true;
   }
}

void NumericConverter::exactnessSeen(char x)
{
    switch ( state ) {
    case ScanState::SS_BEGIN:
        state = ScanState::SS_EXACT;
        break;
    case ScanState::SS_BASE:
        state = ScanState::SS_BASE_AND_EXACT;
        break;
    default:
        state = ScanState::SS_ERROR;
        return;
    }

    if ( 'e' == tolower(x) ) {
        exactness = ExactnessType::ET_EXACT;
    }
    else {
        exactness = ExactnessType::ET_INEXACT;
    }
}

void NumericConverter::baseSeen(char x)
{
    switch ( state ) {
    case ScanState::SS_BEGIN:
        state = ScanState::SS_BASE;
        break;
    case ScanState::SS_EXACT:
        state = ScanState::SS_BASE_AND_EXACT;
        break;
    default:
        state = ScanState::SS_ERROR;
        return;
    }

    switch ( x ) {
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
        state = ScanState::SS_ERROR;
        break;
    }
}

void NumericConverter::handleChar(char c)
{
    if ( '.' == c ) {
        if ( 10 == base && ScanState::SS_INTEGER_PART == state ) {
            state = ScanState::SS_RADIX_POINT;
        }
        else {
            state = ScanState::SS_ERROR;
        }
        exact = false;
        return;
    }

    int digit = convertDigit(*pos);
    if ( -1 == digit ) {
        state = ScanState::SS_ERROR;
        return;
    }

    switch ( state ) {
    case ScanState::SS_BEGIN:
    case ScanState::SS_BASE:
    case ScanState::SS_EXACT:
    case ScanState::SS_BASE_AND_EXACT:
    case ScanState::SS_SIGN:
    case ScanState::SS_INTEGER_PART:
        integerPart = integerPart * base + digit;
        state = ScanState::SS_INTEGER_PART;
        break;

    case ScanState::SS_RADIX_POINT:
    case ScanState::SS_FRACTIONAL_PART:
        divisor /= base;
        fractionalPart = fractionalPart + divisor * digit;
        state = ScanState::SS_FRACTIONAL_PART;
        break;

    default:
        state = ScanState::SS_ERROR;
        break;
    }
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
    if ( ScanState::SS_INTEGER_PART == state ||
         ( ScanState::SS_FRACTIONAL_PART == state &&
           0.0 == fractionalPart )) {
        int rv = (negative ? -1 : 1) * (int)integerPart;
        value = ExpressionFactory::makeInteger(rv, exact);
        state = ScanState::SS_DONE;
    }

    else if ( ScanState::SS_FRACTIONAL_PART == state ) {
        double t = integerPart + fractionalPart;
        double rv = negative ? -t : t;
        value = ExpressionFactory::makeReal(rv, exact);
        state = ScanState::SS_DONE;
    }

    else {
        state = ScanState::SS_ERROR;
    }
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
