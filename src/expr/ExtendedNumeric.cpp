#include "expr/ExtendedNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ScamExpr.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ExtendedNumeric::ExtendedNumeric(ExprHandle expr)
    : expr(expr)
{
    if ( ! expr->isNumeric() ) {
        stringstream s;
        s << "Attempting to make ExtendedNumeric from " << expr->toString();
        throw ScamException(s.str());
    }
}

ExprHandle ExtendedNumeric::get() const
{
    return expr;
}

bool ExtendedNumeric::isNaN() const
{
    return expr->isNaN();
}

bool ExtendedNumeric::isNegInf() const
{
    return expr->isNegInf();
}

bool ExtendedNumeric::isPosInf() const
{
    return expr->isPosInf();
}

//////////////////////////////////////////

bool scam::operator==(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    if ( a.isNaN() || b.isNaN() ) {
        return false;
    }

    const bool aNegInf = a.isNegInf();
    const bool aPosInf = a.isPosInf();
    const bool bNegInf = b.isNegInf();
    const bool bPosInf = b.isPosInf();

    if (( aNegInf && bNegInf ) || ( aPosInf && bPosInf)) {
        return true;
    }
    if ( aNegInf || bNegInf || aPosInf || bPosInf ) {
        return false;
    }

    const bool rv = a.get()->equals(b.get());
    return rv;
}

bool scam::operator!=(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    if ( a.isNaN() || b.isNaN() ) {
        return false;
    }

    const bool rv = ! (a == b);
    return rv;
}

bool scam::operator>(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    if ( a.isNaN() || b.isNaN() ) {
        return false;
    }

    const bool aNegInf = a.isNegInf();
    const bool aPosInf = a.isPosInf();
    const bool bNegInf = b.isNegInf();
    const bool bPosInf = b.isPosInf();

    if ( aPosInf ) {
        const bool rv = ! bPosInf;
        return rv;
    }
    if ( bNegInf ) {
        const bool rv = ! aNegInf;
        return rv;
    }
    if ( aNegInf || bPosInf ) {
        return false;
    }

    const bool rv = a.get()->toReal() > b.get()->toReal();
    return rv;
}

bool scam::operator>=(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    if ( a.isNaN() || b.isNaN() ) {
        return false;
    }
    return (a > b) || (a == b);
}

bool scam::operator<(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    if ( a.isNaN() || b.isNaN() ) {
        return false;
    }

    const bool rv = ! (a >= b);
    return rv;
}

bool scam::operator<=(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    if ( a.isNaN() || b.isNaN() ) {
        return false;
    }

    const bool rv = (a < b) || (a == b);
    return rv;
}
