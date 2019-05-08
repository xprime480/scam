#include "expr/ScamNumeric.hpp"

#include "expr/ExpressionFactory.hpp"

#include <string>

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
}

ScamNumeric * ScamNumeric::makeValue(const string & text)
{
    bool exact = true;
    NumericType type = NumericType::NT_INTEGER;
    bool negative = false;
    double real = 0.0;
    double frac = 0.0;

    const char * pos = text.c_str();
    if ( '+' == *pos ) {
        ++pos;
    }
    else if ( '-' == *pos ) {
        ++pos;
        negative = true;
    }
    if ( '.' == *pos ) {
        // Handle this error -- shouldn't happen
    }

    while ( *pos ) {
        if ( '.' == *pos ) {
            ++pos;
            exact = false;
            break;
        }
        if ( ! isdigit(*pos) ) {
            // Handle this error -- shouldn't happen
        }
        real = real * 10 + (*pos) - '0';
        ++pos;
    }

    double d = 0.1;
    while ( *pos ) {
        if ( ! isdigit(*pos) ) {
            // Handle this error -- shouldn't happen
        }
        if ( '0' != *pos ) {
            type = NumericType::NT_REAL;
        }
        frac = frac + d * ((*pos) -'0');
        d = d / 10;
    }

    if ( NumericType::NT_INTEGER == type ) {
        ExpressionFactory::makeInteger(negative ? -real : real, exact);
    }
    double t = real+frac;
    return ExpressionFactory::makeReal(negative ? -t : t, exact);
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



