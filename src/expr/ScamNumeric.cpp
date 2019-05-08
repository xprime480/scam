#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

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
}

ScamNumeric * ScamNumeric::makeValue(const string & text)
{
    bool exact = true;
    NumericType type = NumericType::NT_INTEGER;
    ExactnessType exactness = ExactnessType::ET_CONTEXT;
    bool negative = false;
    double real = 0.0;
    double frac = 0.0;

    const char * pos = text.c_str();

    while ( '#' == *pos ) {
        switch ( pos[1] ) {
        case 'e': case 'E':
            exactness = ExactnessType::ET_EXACT;
            break;
        case 'i': case 'I':
            exactness = ExactnessType::ET_INEXACT;
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
        if ( ! isdigit(*pos) ) {
            stringstream s;
            s << "ScamNumeric::makeValue: non-digit found: "
              << text;
            throw ScamException(s.str());
        }
        real = real * 10 + (*pos) - '0';
        ++pos;
    }

    double d = 0.1;
    while ( *pos ) {
        if ( ! isdigit(*pos) ) {
            stringstream s;
            s << "ScamNumeric::makeValue: non-digit found: "
              << text;
            throw ScamException(s.str());
        }
        if ( '0' != *pos ) {
            type = NumericType::NT_REAL;
        }
        frac = frac + d * ((*pos) -'0');
        ++pos;
        d = d / 10;
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
