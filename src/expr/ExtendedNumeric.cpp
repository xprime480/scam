#include "expr/ExtendedNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "util/NumericConverter.hpp"

#include <cmath>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern ScamValue signCheckRI(ScamValue r, ScamValue b);
}

ExtendedNumeric::ExtendedNumeric(ConstScamValue expr)
    : expr(expr)
{
    if ( ! TypePredicates::isNumeric(expr) ) {
        stringstream s;
        s << "Attempting to make ExtendedNumeric from " << writeValue(expr);
        throw ScamException(s.str());
    }
}

ScamValue ExtendedNumeric::get() const
{
    return const_cast<ScamValue>(expr);
}

bool ExtendedNumeric::isNaN() const
{
    return TypePredicates::isNaN(expr);
}

bool ExtendedNumeric::isNegInf() const
{
    return TypePredicates::isNegInf(expr);
}

bool ExtendedNumeric::isPosInf() const
{
    return TypePredicates::isPosInf(expr);
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

    ScamValue hA = a.get();
    ScamValue hB = b.get();
    if ( (TypePredicates::isComplex(hA) && ! TypePredicates::isReal(hA)) ||
         (TypePredicates::isComplex(hB) && ! TypePredicates::isReal(hB)) ) {
        return false;
    }

    const bool rv = asDouble(hA) > asDouble(hB);
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

    ScamValue hA = a.get();
    ScamValue hB = b.get();
    if ( (TypePredicates::isComplex(hA) && ! TypePredicates::isReal(hA)) ||
         (TypePredicates::isComplex(hB) && ! TypePredicates::isReal(hB)) ) {
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
    ScamValue expr { nullptr };

    if ( ! a.isSpecialNumeric() && ! b.isSpecialNumeric() ) {
        const ScamValue xA = a.get();
        const ScamValue xB = b.get();
        const bool isInt =
            TypePredicates::isInteger(xA) && TypePredicates::isInteger(xB);
        const bool isRational =
            TypePredicates::isRational(xA) && TypePredicates::isRational(xB);
        const bool isReal =
            TypePredicates::isReal(xA) && TypePredicates::isReal(xB);
        const bool isExact =
            TypePredicates::isExact(xA) && TypePredicates::isExact(xB);

        if ( isInt ) {
            const int rv = asInteger(xA) + asInteger(xB);
            expr = ExpressionFactory::makeInteger(rv, isExact);
        }
        else if ( isRational ) {
            const RationalPair ratA = asRational(xA);
            const RationalPair ratB = asRational(xB);
            const int num = ratA.num * ratB.den + ratB.num * ratA.den;
            const int den = ratA.den * ratB.den;
            expr = ExpressionFactory::makeRational(num, den, isExact);
        }
        else if ( isReal ) {
            const double rv = asDouble(xA) + asDouble(xB);
            expr = ExpressionFactory::makeReal(rv, isExact);
        }
        else {
            ExtendedNumeric rA(xA->realPart());
            ExtendedNumeric rB(xB->realPart());
            ExtendedNumeric iA(xA->imagPart());
            ExtendedNumeric iB(xB->imagPart());

            ExtendedNumeric rC = rA + rB;
            ExtendedNumeric iC = iA + iB;

            expr = ExpressionFactory::makeComplex(rC.get(), iC.get());
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

    ExtendedNumeric tmp(NumericConverter::simplify(expr));
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
    ScamValue expr { nullptr };

    if ( ! a.isSpecialNumeric() && ! b.isSpecialNumeric() ) {
        ExtendedNumeric minusOne(ExpressionFactory::makeInteger(-1, true));
        ExtendedNumeric minusB = minusOne * b;
        return a + minusB;
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

    ExtendedNumeric tmp(NumericConverter::simplify(expr));
    return tmp;
}

ExtendedNumeric
scam::operator*(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue xA = a.get();
    ScamValue xB = b.get();
    ScamValue expr { nullptr };

    if ( ! a.isSpecialNumeric() && ! b.isSpecialNumeric() ) {
        const bool isInt =
            TypePredicates::isInteger(xA) && TypePredicates::isInteger(xB);
        const bool isRational =
            TypePredicates::isRational(xA) && TypePredicates::isRational(xB);
        const bool isReal =
            TypePredicates::isReal(xA) && TypePredicates::isReal(xB);
        const bool isExact =
            TypePredicates::isExact(xA) && TypePredicates::isExact(xB);

        if ( isInt ) {
            const int rv = asInteger(xA) * asInteger(xB);
            expr = ExpressionFactory::makeInteger(rv, isExact);
        }
        else if ( isRational ) {
            const RationalPair ratA = asRational(xA);
            const RationalPair ratB = asRational(xB);
            const int num = ratA.num * ratB.num;
            const int den = ratA.den * ratB.den;
            expr = ExpressionFactory::makeRational(num, den, isExact);
        }
        else if ( isReal ) {
            const double rv = asDouble(xA) * asDouble(xB);
            expr = ExpressionFactory::makeReal(rv, isExact);
        }
        else {
            ExtendedNumeric rA(xA->realPart());
            ExtendedNumeric rB(xB->realPart());
            ExtendedNumeric iA(xA->imagPart());
            ExtendedNumeric iB(xB->imagPart());

            ExtendedNumeric rC = rA * rB - iA * iB;
            ExtendedNumeric iC = rA * iB + iA * rB;

            expr = ExpressionFactory::makeComplex(rC.get(), iC.get());
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

    ExtendedNumeric tmp(NumericConverter::simplify(expr));
    return tmp;
}

ExtendedNumeric
scam::operator/(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    const ScamValue xA = a.get();
    const ScamValue xB = b.get();
    ScamValue expr { nullptr };

    if ( ! a.isSpecialNumeric() && ! b.isSpecialNumeric() ) {
        const bool isInt =
            TypePredicates::isInteger(xA) && TypePredicates::isInteger(xB);
        const bool isRational =
            TypePredicates::isRational(xA) && TypePredicates::isRational(xB);
        const bool isReal =
            TypePredicates::isReal(xA) && TypePredicates::isReal(xB);
        const bool isExact =
            TypePredicates::isExact(xA) && TypePredicates::isExact(xB);

        if ( isInt ) {
            const int iA = asInteger(xA);
            const int iB = asInteger(xB);
            if ( 0 == (iA % iB) ) {
                const int rv =  iA / iB;
                expr = ExpressionFactory::makeInteger(rv, isExact);
            }
            else if ( ::abs(iA) <= 1e9 && ::abs(iB) <= 1e9 ) {
                expr = ExpressionFactory::makeRational(iA, iB, isExact);
            }
            else {
                const double rv = (double) iA / (double) iB;
                expr = ExpressionFactory::makeReal(rv, isExact);
            }
        }
        else if ( isRational ) {
            const RationalPair ratB = asRational(xB);
            int s = ratB.num < 0 ? -1 : 1;
            ScamValue recipricolB =
                ExpressionFactory::makeRational(s * ratB.den,
                                                ::abs(ratB.num),
                                                isExact);
            ExtendedNumeric newB(recipricolB);
            expr = (a * newB).get();
        }
        else if ( isReal ) {
            const double rv = asDouble(xA) / asDouble(xB);
            expr = ExpressionFactory::makeReal(rv, isExact);
        }
        else {
            ExtendedNumeric rB(xB->realPart());
            ExtendedNumeric iB(xB->imagPart());
            ExtendedNumeric iBNeg = -iB;
            ExtendedNumeric bConj(ExpressionFactory::makeComplex(rB.get(),
                                                                 iBNeg.get()));

            ExtendedNumeric num = a * bConj;
            ExtendedNumeric den = b * bConj;

            if ( den.isNaN() ) {
                expr = den.get();
            }
            else {
                ExtendedNumeric rNum(num.get()->realPart());
                ExtendedNumeric iNum(num.get()->imagPart());
                ExtendedNumeric rC = rNum / den;
                ExtendedNumeric iC = iNum / den;

                expr = ExpressionFactory::makeComplex(rC.get(), iC.get());
            }
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
            if ( aNegInf || aPosInf || (0 != asDouble(xA)) ) {
                expr = ExpressionFactory::makeNaN();
            }
            else {
                const bool ex = TypePredicates::isExact(xA);
                expr = ExpressionFactory::makeInteger(0, ex);
            }
        }
        else {
            expr = signCheckRI(xB, xA);
        }
    }

    ExtendedNumeric tmp(NumericConverter::simplify(expr));
    return tmp;
}

ExtendedNumeric
scam::operator%(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    const ScamValue xA = a.get();
    const ScamValue xB = b.get();
    ScamValue expr { nullptr };

    if ( ! a.isSpecialNumeric() && ! b.isSpecialNumeric() ) {
        const bool isInt =
            TypePredicates::isInteger(xA) && TypePredicates::isInteger(xB);
        const bool isRational =
            TypePredicates::isRational(xA) && TypePredicates::isRational(xB);
        const bool isExact =
            TypePredicates::isExact(xA) && TypePredicates::isExact(xB);

        if ( isInt ) {
            const int rv = asInteger(xA) % asInteger(xB);
            expr = ExpressionFactory::makeInteger(rv, isExact);
        }
        else if ( isRational ) {
            ExtendedNumeric quotient = a / b;
            int q = (int) (0.0000001 + asDouble(quotient.get()));
            ExtendedNumeric x(ExpressionFactory::makeInteger(q, isExact));
            ExtendedNumeric wp = x * b;
            return a - wp;
        }
        else {
            const double rv = fmod(asDouble(xA), asDouble(xB));
            expr = ExpressionFactory::makeReal(rv, isExact);
        }
    }
    else {
        if ( ! a.isSpecialNumeric() && 0 == asDouble(xA) ) {
            const bool ex = TypePredicates::isExact(xA);
            expr = ExpressionFactory::makeInteger(0, ex);
        }
        else {
            expr = ExpressionFactory::makeNaN();
        }
    }

    ExtendedNumeric tmp(NumericConverter::simplify(expr));
    return tmp;
}

///////////////////////////////////////

namespace
{
    ScamValue signCheckRI(ScamValue r, ScamValue i)
    {
        const double rVal = asDouble(r);
        if ( 0.0 == rVal ) {
            const bool ex = TypePredicates::isExact(r);
            return ExpressionFactory::makeInteger(0, ex);
        }

        if ( rVal > 0 ) {
            return i;
        }

        if ( TypePredicates::isPosInf(i) ) {
            return ExpressionFactory::makeNegInf();
        }

        return ExpressionFactory::makePosInf();
    }
}
