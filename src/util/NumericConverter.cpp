#include "util/NumericConverter.hpp"

#include "expr/ExtendedNumeric.hpp"
#include "input/CharStream.hpp"
#include "input/StringTokenizer.hpp"
#include "value/ScamNumeric.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

#include <cmath>
#include <cstring>
#include <limits>

using namespace scam;
using namespace std;

NumericConverter::NumericConverter(CharStream & stream)
    : stream(stream)
    , value(makeNothing())
    , base(10)
    , exactness(ExactnessType::ET_CONTEXT)
{
    scanNum();
}

ScamValue includeSign(int sign, ScamValue expr)
{
    if ( sign < 0 ) {
        ExtendedNumeric lhs(makeInteger(sign, true));
        ExtendedNumeric rhs(expr);
        ExtendedNumeric result = lhs * rhs;
        expr = result.get();
    }
    return expr;
}

ScamValue NumericConverter::simplify(ScamValue value)
{
    if ( ! isNumeric(value) ) {
        return value;
    }

    if ( isSpecialNumeric(value) ) {
        return value;
    }

    if ( isPureComplex(value) ) {
        ScamValue imag = simplify(imagPart(value));
        if ( isInteger(imag) && 0 == asInteger(imag) ) {
            value = realPart(value);
        }
    }

    if ( isReal(value) && (! isRational(value)) ) {
        double v = asDouble(value);
        if ( ::fabs(v) >= (double) numeric_limits<int>::max() ) {
            return value;
        }
        double frac = ::fmod(v, 1.0);
        if ( 0.0 == frac ) {
            const bool ex = isExact(value);
            value = makeRational((int)v, 1, ex);
        }
        else {
            return value;
        }
    }

    if ( isRational(value) && ! isInteger(value) ) {
        const RationalPair v = asRational(value);
        if ( 1 == v.den ) {
            const bool ex = isExact(value);
            value = makeInteger(v.num, ex);
        }
    }

    return value;
}

ScamValue NumericConverter::getValue() const
{
    return value;
}

void NumericConverter::scanNum()
{
    scanPrefix();
    scanComplex();

    if ( ! isDelimiter(stream.peek()) ) {
        value = makeNothing();
    }
}

void NumericConverter::scanComplex()
{
    ScamValue zero = makeIntegerWithExactness("0");
    ScamValue one  = makeIntegerWithExactness("1");

    ScamValue rv = makeNothing();
    PositionType original = stream.getPos();

    ScamValue real = scanInfNan();
    ScamValue imag = makeNothing();

    if ( isNothing(real) ) { // not infnan
        int sign = scanSign(false);
        if ( 0 == sign ) {
            rv = imag;
        }
        else {
            real = zero;
            imag = scanUReal();
            if ( isNothing(imag) ) {
                imag = one;
            }
            if ( 'i' == tolower(stream.peek()) ) {
                stream.advance();
                imag = includeSign(sign, imag);
                rv = makeComplex(real, imag);
            }
        }
    }
    else {                      // infnan
        imag = scanReal();
        if ( 'i' == tolower(stream.peek()) ) {
            stream.advance();
            if ( isNothing(imag) ) {
                imag = real;
                real = zero;
            }
            rv = makeComplex(real, imag);
        }
        else {
            rv = real;
        }
    }

    if ( isNothing(rv) ) { // not infnan and not pure imaginary
        // try again from the start for real [+/- imag]
        stream.setPos(original);
        real = scanReal();
        if ( isNothing(real) ) {
            rv = real;          // not any kind of number
        }
        else {
            original = stream.getPos(); // save in case rest does not compute
            int sign = 1;
            imag = scanInfNan();
            if ( isNothing(imag) ) {
                sign = scanSign();
                if ( 0 == sign ) {
                    imag = zero;
                }
                else {
                    imag = scanUReal();
                    if ( isNothing(imag) ) {
                        imag = one;
                    }
                }
            }

            if ( 'i' == tolower(stream.peek()) ) {
                stream.advance();
                imag = includeSign(sign, imag);
            }
            else {
                stream.setPos(original);
                rv = real;
            }

            if ( isNothing(rv) ) {
                rv = makeComplex(real, imag);
            }
        }
    }

    if ( isReal(rv) ) {
        if ( '@' == stream.peek() ) {
            PositionType tmp = stream.getPos();
            stream.advance();
            imag = scanReal();
            if ( isNothing(imag) ) {
                stream.setPos(tmp);
            }
            else {
                rv = makeComplexPolar(rv, imag);
            }
        }
    }

    value = simplify(rv);
}

ScamValue NumericConverter::scanReal()
{
    ScamValue rv = scanInfNan();
    if ( ! isNothing(rv) ) {
        return rv;
    }

    PositionType original = stream.getPos();

    const int sign = scanSign();
    rv = scanUReal();

    if ( isNothing(rv) ) {
        stream.setPos(original);
        return rv;
    }

    rv = includeSign(sign, rv);
    return rv;
}

ScamValue NumericConverter::scanUReal()
{
    PositionType original = stream.getPos();

    ScamValue rv10 { makeNothing() };
    PositionType pos10 = stream.getPos();
    if ( 10 == base ) {
        rv10 = scanDecimal();
        if ( ! isNothing(rv10) ) {
            pos10 = stream.getPos();
        }
    }

    stream.setPos(original);

    ScamValue rvN = scanUInteger();
    if ( isNothing(rvN) ) {
        stream.setPos(pos10);
        return rv10;
    }

    PositionType posN = stream.getPos();
    original = stream.getPos();
    if ( '/' != stream.peek() ) {
        if ( posN > pos10 ) {
            stream.setPos(posN);
            return rvN;
        }
        else {
            stream.setPos(pos10);
            return rv10;
        }
    }

    stream.advance();

    ScamValue rvD = scanUInteger();
    if ( isNothing(rvD) ) {
        stream.setPos(original);
        return ( posN > pos10 ) ? rvN : rv10;
    }

    ScamValue rv = makeRationalWithExactness(asInteger(rvN), asInteger(rvD));
    return rv;
}

ScamValue NumericConverter::scanDecimal()
{
    PositionType original = stream.getPos();
    ScamValue rv { makeNothing() };

    if ( scanRadixPoint() ) {
        rv = makeFraction(1);
        if ( isNothing(rv) ) {
            stream.setPos(original);
            return rv;
        }
    }
    else {
        rv = scanUInteger();
        if ( isNothing(rv) ) {
            stream.setPos(original);
            return rv;
        }

        if ( scanRadixPoint() ) {
            ScamValue fp = makeFraction(0);
            if ( ! isNothing(rv) ) {
                ExtendedNumeric lhs(rv);
                ExtendedNumeric rhs(fp);
                ExtendedNumeric result = lhs + rhs;
                rv = result.get();
            }
        }
    }

    ScamValue suffix = scanSuffix();
    if ( isReal(suffix) ) {
        ExtendedNumeric lhs(rv);
        ExtendedNumeric rhs(suffix);
        ExtendedNumeric result = lhs * rhs;
        rv = result.get();
    }

    return rv;
}

ScamValue NumericConverter::scanUInteger()
{
    PositionType original = stream.getPos();

    size_t count { 0 };
    stringstream s;

    while ( char c = stream.peek() ) {
        int digit = convertDigit(c);
        if ( -1 == digit ) {
            break;
        }

        s  << c;
        stream.advance();
        ++count;
    }

    if ( 0 == count ) {
        stream.setPos(original);
        return makeNothing();
    }

    return makeIntegerWithExactness(stream.strBetween(original));
}

void NumericConverter::scanPrefix()
{
    while ( '#' == stream.peek() ) {
        PositionType original = stream.getPos();
        stream.advance();
        switch ( char c = stream.peek() ) {
        case 'e': case 'E':
        case 'i': case 'I':
            exactnessSeen(c);
            break;

        case 'b': case 'B':
        case 'o': case 'O':
        case 'd': case 'D':
        case 'x': case 'X':
            baseSeen(c);
            break;

        default:
            stream.setPos(original);
            return;
            break;
        }

        stream.advance();
    }
}

ScamValue NumericConverter::scanInfNan()
{
    ScamValue rv { makeNothing() };

    string peek6 = stream.strPeek(6);
    if ( peek6 == "+nan.0" || peek6 == "-nan.0" ) {
        rv = makeNaN();
    }
    else if ( peek6 == "+inf.0" ) {
        rv = makePosInf();
    }
    else if ( peek6 == "-inf.0" ) {
        rv = makeNegInf();
    }
    else {
        return rv;
    }

    stream.advance(6);
    return rv;
}

ScamValue NumericConverter::scanSuffix()
{
    PositionType original = stream.getPos();
    ScamValue rv { makeNothing() };

    if ( 'e' == tolower(stream.peek()) ) {
        stream.advance();
        int sign = scanSign();
        ScamValue value = scanUInteger();
        if ( isInteger(value) ) {
            double suffix = makeMultiplier(sign * asInteger(value));
            rv = makeRealWithExactness(suffix);
        }
    }

    if ( isNothing(rv) ) {
        stream.setPos(original);
    }

    return rv;
}

int NumericConverter::scanSign(bool optional)
{
    char c = stream.peek();
    if ( '+' == c ) {
        stream.advance();
        return 1;
    }
    else if ( '-' == c ) {
        stream.advance();
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

ScamValue NumericConverter::makeFraction(unsigned minCount)
{
    PositionType original = stream.getPos();
    ScamValue rv = scanUInteger();

    const string text = stream.strBetween(original);
    unsigned count = text.size();
    if ( count < minCount ) {
        stream.setPos(original);
        return makeNothing();
    }

    bool makeExact = exactness == ExactnessType::ET_EXACT;

    if ( isNothing(rv) ) {
        return makeInteger(0, makeExact);
    }


    if ( count <= 6 ) {
        int value = asInteger(rv);
        double multiplier = makeMultiplier(count);
        return makeRational(value, multiplier, makeExact);
    }
    else {
        stream.setPos(original);
        double divisor = 1.0;
        double value = 0.0;

        while ( isdigit(stream.peek() ) ) {
            divisor /= base;
            value += divisor * convertDigit(stream.peek());
            stream.advance();
        }

        return makeRealWithExactness(value);
    }
}

bool NumericConverter::scanRadixPoint()
{
    if ( '.' == stream.peek() ) {
        stream.advance();
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

ScamValue
NumericConverter::makeComplexPolar(ScamValue r, ScamValue theta) const
{
    ScamValue nan = makeNaN();
    ScamValue real;
    ScamValue imag;

    if ( isSpecialNumeric(r) ) {
        real = imag = nan;
    }
    else if ( 0.0 == asDouble(r) ) {
        real = imag = r;
    }
    else if ( isSpecialNumeric(theta) ) {
        real = imag = nan;
    }
    else {
        const bool exact = isExact(r) && isExact(theta);
        const double radius = asDouble(r);
        const double cosine = ::cos(asDouble(theta));
        const double sine   = ::sin(asDouble(theta));

        const double x = radius * cosine;
        const double y = radius * sine;
        real = makeReal(x, exact);
        imag = makeReal(y, exact);
    }

    ScamValue rv = makeComplex(real, imag);
    return rv;
}

ScamValue NumericConverter::makeRealWithExactness(double value) const
{
    bool makeExact = exactness == ExactnessType::ET_EXACT;
    return simplify(makeReal(value, makeExact));
}

ScamValue NumericConverter::makeRationalWithExactness(int num, int den) const
{
    bool makeExact = ! (exactness == ExactnessType::ET_INEXACT);
    return makeRational(num, den, makeExact);
}

ScamValue NumericConverter::makeIntegerWithExactness(const string & value) const
{
    bool makeExact = ! (exactness == ExactnessType::ET_INEXACT);
    return makeInteger(value, makeExact);
}
