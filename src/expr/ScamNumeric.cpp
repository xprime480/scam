#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

#include <cctype>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    enum class NumericType : unsigned char
        {
         NT_INTEGER,
         NT_RATIONAL,
         NT_REAL,
         NT_COMPLEX
        };

    enum class ExactnessType : unsigned char
        {
         ET_EXACT,
         ET_INEXACT,
         ET_CONTEXT
        };

    extern int convertDigit(int base, char digit);
}

ScamNumeric * ScamNumeric::makeValue(const string & text)
{
    bool exact = true;
    NumericType type = NumericType::NT_INTEGER;
    ExactnessType exactness = ExactnessType::ET_CONTEXT;
    bool negative = false;
    double real = 0.0;
    double frac = 0.0;
    int base = 10;

    const char * pos = text.c_str();

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
    if ( '.' == *pos ) {
        stringstream s;
        s << "ScamNumeric::makeValue: decimal point before digits: "
          << text;
        throw ScamException(s.str());
    }

    while ( *pos ) {
        if ( '.' == *pos ) {
            ++pos;
            exact = false;
            break;
        }
        int digit = convertDigit(base, *pos);
        if ( -1 == digit ) {
            stringstream s;
            s << "ScamNumeric::makeValue: non-digit found: "
              << text;
            throw ScamException(s.str());
        }
        real = real * base + digit;
        ++pos;
    }

    double d = 1.0 / base;
    while ( *pos ) {
        int digit = convertDigit(base, *pos);
        if ( -1 == digit ) {
            stringstream s;
            s << "ScamNumeric::makeValue: non-digit found: "
              << text;
            throw ScamException(s.str());
        }
        if ( '0' != *pos ) {
            type = NumericType::NT_REAL;
        }
        frac = frac + d * digit;
        ++pos;
        d = d / base;
    }

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

    if ( NumericType::NT_INTEGER == type ) {
        int value = (negative ? -1 : 1) * (int)real;
        return ExpressionFactory::makeInteger(value, exact);
    }

    double t = real+frac;
    double value = negative ? -t : t;
    return ExpressionFactory::makeReal(value, exact);
}

ScamNumeric::ScamNumeric(bool exact)
    : exact(exact)
{
}

bool ScamNumeric::isNumeric() const
{
    return true;
}

bool ScamNumeric::equals(ConstExprHandle expr) const
{
    if ( ! expr->isNumeric() ) {
        return false;
    }

    const ScamNumeric * that = dynamic_cast<const ScamNumeric *>(expr);

    return ( ::abs(this->realPart() - that->realPart()) < 1e-9 &&
             ::abs(this->imagPart() - that->imagPart()) < 1e-9 );
}

bool ScamNumeric::isExact() const
{
    return exact;
}

namespace
{
    int convertDigit(int base, char digit)
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
}
