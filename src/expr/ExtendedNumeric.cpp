#include "expr/ExtendedNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <cmath>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern ExprHandle signCheckRI(ExprHandle r, ExprHandle b);
}

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

bool ExtendedNumeric::isSpecialNumeric() const
{
    return isNaN() || isNegInf() || isPosInf();
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

ExtendedNumeric
scam::operator+(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ExprHandle expr { nullptr };

    if ( ! a.isSpecialNumeric() && ! b.isSpecialNumeric() ) {
        const ExprHandle xA = a.get();
        const ExprHandle xB = b.get();
        const bool isInt = xA->isInteger() && xB->isInteger();
        const bool isExact = xA->isExact() && xB->isExact();

        if ( isInt ) {
            const int rv = xA->toInteger() + xB->toInteger();
            expr = ExpressionFactory::makeInteger(rv, isExact);
        }
        else {
            const double rv = xA->toReal() + xB->toReal();
            expr = ExpressionFactory::makeReal(rv, isExact);
        }
    }
    else if ( a.isNaN() || b.isNaN() ) {
        expr = ExpressionFactory::makeNaN();
    }
    else {
        const bool aNegInf = a.isNegInf();
        const bool aPosInf = a.isPosInf();
        const bool bNegInf = b.isNegInf();
        const bool bPosInf = b.isPosInf();

        const int code = (((aNegInf ? 1 : 0) << 3) |
                          ((aPosInf ? 1 : 0) << 2) |
                          ((bNegInf ? 1 : 0) << 1) |
                          ((bPosInf ? 1 : 0) << 0));

        switch ( code ) {
        case 6: /* +inf.0 + -inf.0 */
        case 9: /* -inf.0 + +inf.0 */
            expr = ExpressionFactory::makeNaN();
            break;

        case 2: /* N + -inf.0 */
        case 8: /* -inf.0 + N */
        case 10: /* -inf.0 + -inf.0 */
            expr = ExpressionFactory::makeNegInf();
            break;

        case 1: /* N + +inf.0 */
        case 4: /* +inf.0 + N */
        case 5: /* +inf.0 + +inf.0 */
            expr = ExpressionFactory::makePosInf();
            break;

        default:
            throw ScamException("internal error in ExtendedNumeric subtraction");
            break;
        }
    }

    ExtendedNumeric tmp(expr);
    return tmp;
}

ExtendedNumeric scam::operator-(const ExtendedNumeric & a)
{
    ExtendedNumeric zero(ExpressionFactory::makeInteger(0, true));
    return zero - a;
}

ExtendedNumeric
scam::operator-(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ExprHandle expr { nullptr };

    if ( ! a.isSpecialNumeric() && ! b.isSpecialNumeric() ) {
        const ExprHandle xA = a.get();
        const ExprHandle xB = b.get();
        const bool isInt = xA->isInteger() && xB->isInteger();
        const bool isExact = xA->isExact() && xB->isExact();

        if ( isInt ) {
            const int rv = xA->toInteger() - xB->toInteger();
            expr = ExpressionFactory::makeInteger(rv, isExact);
        }
        else {
            const double rv = xA->toReal() - xB->toReal();
            expr = ExpressionFactory::makeReal(rv, isExact);
        }
    }
    else if ( a.isNaN() || b.isNaN() ) {
        expr = ExpressionFactory::makeNaN();
    }
    else {
        const bool aNegInf = a.isNegInf();
        const bool aPosInf = a.isPosInf();
        const bool bNegInf = b.isNegInf();
        const bool bPosInf = b.isPosInf();

        const int code = (((aNegInf ? 1 : 0) << 3) |
                          ((aPosInf ? 1 : 0) << 2) |
                          ((bNegInf ? 1 : 0) << 1) |
                          ((bPosInf ? 1 : 0) << 0));

        switch ( code ) {
        case 5: /* +inf.0 - +inf.0 */
        case 10: /* -inf.0 - -inf.0 */
            expr = ExpressionFactory::makeNaN();
            break;

        case 1: /* N - +inf.0 */
        case 8: /* -inf.0 - N */
        case 9: /* -inf.0 - +inf.0 */
            expr = ExpressionFactory::makeNegInf();
            break;

        case 2: /* N - -inf.0 */
        case 4: /* +inf.0 - N */
        case 6: /* +inf.0 - -inf.0 */
            expr = ExpressionFactory::makePosInf();
            break;

        default:
            throw ScamException("internal error in ExtendedNumeric subtraction");
            break;
        }
    }

    ExtendedNumeric tmp(expr);
    return tmp;
}

ExtendedNumeric
scam::operator*(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ExprHandle xA = a.get();
    ExprHandle xB = b.get();
    ExprHandle expr { nullptr };

    if ( ! a.isSpecialNumeric() && ! b.isSpecialNumeric() ) {
        const bool isInt = xA->isInteger() && xB->isInteger();
        const bool isExact = xA->isExact() && xB->isExact();

        if ( isInt ) {
            const int rv = xA->toInteger() * xB->toInteger();
            expr = ExpressionFactory::makeInteger(rv, isExact);
        }
        else {
            const double rv = xA->toReal() * xB->toReal();
            expr = ExpressionFactory::makeReal(rv, isExact);
        }
    }
    else if ( a.isNaN() || b.isNaN() ) {
        expr = ExpressionFactory::makeNaN();
    }
    else {
        const bool aNegInf = a.isNegInf();
        const bool aPosInf = a.isPosInf();
        const bool bNegInf = b.isNegInf();
        const bool bPosInf = b.isPosInf();

        const int code = (((aNegInf ? 1 : 0) << 3) |
                          ((aPosInf ? 1 : 0) << 2) |
                          ((bNegInf ? 1 : 0) << 1) |
                          ((bPosInf ? 1 : 0) << 0));

        switch ( code ) {
        case 1: /* N * +inf.0 */
        case 2: /* N * -inf.0 */
            expr = signCheckRI(xA, xB);
            break;

        case 4: /* +inf.0 * N */
        case 8: /* -inf.0 * N */
            expr = signCheckRI(xB, xA);
            break;

        case 6: /* +inf.0 * -inf.0 */
        case 9: /* -inf.0 * +inf.0 */
            expr = ExpressionFactory::makeNegInf();
            break;

        case 5: /* +inf.0 * +inf.0 */
        case 10: /* -inf.0 * -inf.0 */
            expr = ExpressionFactory::makePosInf();
            break;

        default:
            throw ScamException("internal error in ExtendedNumeric multiplication");
            break;
        }
    }

    ExtendedNumeric tmp(expr);
    return tmp;
}

ExtendedNumeric
scam::operator/(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    const ExprHandle xA = a.get();
    const ExprHandle xB = b.get();
    ExprHandle expr { nullptr };

    if ( ! a.isSpecialNumeric() && ! b.isSpecialNumeric() ) {
        const bool isInt = xA->isInteger() && xB->isInteger();
        const bool isExact = xA->isExact() && xB->isExact();

        if ( isInt ) {
            const int iA = xA->toInteger();
            const int iB = xB->toInteger();
            if ( 0 == (iA % iB) ) {
                const int rv =  iA / iB;
                expr = ExpressionFactory::makeInteger(rv, isExact);
            }
            else {
                const double rv = (double) iA / (double) iB;
                expr = ExpressionFactory::makeReal(rv, isExact);
            }
        }
        else {
            const double rv = xA->toReal() / xB->toReal();
            expr = ExpressionFactory::makeReal(rv, isExact);
        }
    }
    else if ( a.isNaN() || b.isNaN() ) {
        expr = ExpressionFactory::makeNaN();
    }
    else {
        const bool aNegInf = a.isNegInf();
        const bool aPosInf = a.isPosInf();
        const bool bNegInf = b.isNegInf();
        const bool bPosInf = b.isPosInf();

        if ( bNegInf || bPosInf ) {
            if ( aNegInf || aPosInf || (0 != xA->toReal()) ) {
                expr = ExpressionFactory::makeNaN();
            }
            else {
                expr = ExpressionFactory::makeInteger(0, xA->isExact());
            }
        }
        else {
            expr = signCheckRI(xB, xA);
        }
    }

    ExtendedNumeric tmp(expr);
    return tmp;
}

ExtendedNumeric
scam::operator%(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    const ExprHandle xA = a.get();
    const ExprHandle xB = b.get();
    ExprHandle expr { nullptr };

    if ( ! a.isSpecialNumeric() && ! b.isSpecialNumeric() ) {
        const bool isInt = xA->isInteger() && xB->isInteger();
        const bool isExact = xA->isExact() && xB->isExact();

        if ( isInt ) {
            const int rv = xA->toInteger() % xB->toInteger();
            expr = ExpressionFactory::makeInteger(rv, isExact);
        }
        else {
            const double rv = fmod(xA->toReal(), xB->toReal());
            expr = ExpressionFactory::makeReal(rv, isExact);
        }
    }
    else {
        if ( ! a.isSpecialNumeric() && 0 == xA->toReal() ) {
            expr = ExpressionFactory::makeInteger(0, xA->isExact());
        }
        else {
            expr = ExpressionFactory::makeNaN();
        }
    }

    ExtendedNumeric tmp(expr);
    return tmp;
}

///////////////////////////////////////

namespace
{
    ExprHandle signCheckRI(ExprHandle r, ExprHandle i)
    {
        const double rVal = r->toReal();
        if ( 0.0 == rVal ) {
            return ExpressionFactory::makeInteger(0, r->isExact());
        }

        if ( rVal > 0 ) {
            return i;
        }

        if ( i->isPosInf() ) {
            return ExpressionFactory::makeNegInf();
        }

        return ExpressionFactory::makePosInf();
    }
}
