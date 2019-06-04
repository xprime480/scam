#include "expr/ExtendedNumeric.hpp"

#include "ScamException.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamNumeric.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/NumericConverter.hpp"

#include <cmath>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    struct JointValues
    {
        bool special;
        bool complex;
        bool real;
        bool rational;
        bool integer;
        bool exact;
    };

    extern ScamValue signCheckRI(ScamValue r, ScamValue b);
    extern JointValues computeJoint(ScamValue lhs, ScamValue rhs);
}

ExtendedNumeric::ExtendedNumeric(ScamValue expr)
    : expr(expr)
{
    if ( ! isNumeric(expr) ) {
        stringstream s;
        s << "Attempting to make ExtendedNumeric from " << writeValue(expr);
        throw ScamException(s.str());
    }
}

ScamValue ExtendedNumeric::get() const
{
    return expr;
}

//////////////////////////////////////////

bool scam::operator==(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue dA = a.get();
    ScamValue dB = b.get();

    if ( isNaN(dA) || isNaN(dB) ) {
        return false;
    }

    const bool aNegInf = isNegInf(dA);
    const bool aPosInf = isPosInf(dA);
    const bool bNegInf = isNegInf(dB);
    const bool bPosInf = isPosInf(dB);

    if (( aNegInf && bNegInf ) || ( aPosInf && bPosInf)) {
        return true;
    }
    if ( aNegInf || bNegInf || aPosInf || bPosInf ) {
        return false;
    }

    const bool rv = equals(dA, dB);
    return rv;
}

bool scam::operator!=(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue dA = a.get();
    ScamValue dB = b.get();

    if ( isNaN(dA) || isNaN(dB) ) {
        return false;
    }

    const bool rv = ! (a == b);
    return rv;
}

bool scam::operator>(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue dA = a.get();
    ScamValue dB = b.get();

    if ( isNaN(dA) || isNaN(dB) ) {
        return false;
    }

    const bool aNegInf = isNegInf(dA);
    const bool aPosInf = isPosInf(dA);
    const bool bNegInf = isNegInf(dB);
    const bool bPosInf = isPosInf(dB);

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

    if ( isPureComplex(dA) || isPureComplex(dB) ) {
        return false;
    }

    const bool rv = asDouble(dA) > asDouble(dB);
    return rv;
}

bool scam::operator>=(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue dA = a.get();
    ScamValue dB = b.get();

    if ( isNaN(dA) || isNaN(dB) ) {
        return false;
    }

    return (a > b) || (a == b);
}

bool scam::operator<(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue dA = a.get();
    ScamValue dB = b.get();

    if ( isNaN(dA) || isNaN(dB) ) {
        return false;
    }

    if ( isPureComplex(dA) || isPureComplex(dB) ) {
        return false;
    }

    const bool rv = ! (a >= b);
    return rv;
}

bool scam::operator<=(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue dA = a.get();
    ScamValue dB = b.get();

    if ( isNaN(dA) || isNaN(dB) ) {
        return false;
    }

    const bool rv = (a < b) || (a == b);
    return rv;
}

ExtendedNumeric
scam::operator+(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue dA = a.get();
    ScamValue dB = b.get();
    ScamValue expr { nullptr };

    const auto joint = computeJoint(dA, dB);
    if ( ! joint.special ) {
        if ( joint.integer ) {
            const int rv = asInteger(dA) + asInteger(dB);
            expr = makeInteger(rv, joint.exact);
        }
        else if ( joint.rational ) {
            const RationalPair ratA = asRational(dA);
            const RationalPair ratB = asRational(dB);
            const int num = ratA.num * ratB.den + ratB.num * ratA.den;
            const int den = ratA.den * ratB.den;
            expr = makeRational(num, den, joint.exact);
        }
        else if ( joint.real ) {
            const double rv = asDouble(dA) + asDouble(dB);
            expr = makeReal(rv, joint.exact);
        }
        else {
            ExtendedNumeric rA(realPart(dA));
            ExtendedNumeric rB(realPart(dB));
            ExtendedNumeric iA(imagPart(dA));
            ExtendedNumeric iB(imagPart(dB));

            ExtendedNumeric rC = rA + rB;
            ExtendedNumeric iC = iA + iB;

            expr = makeComplex(rC.get(), iC.get());
        }
    }
    else if ( isNaN(dA) || isNaN(dB) ) {
        expr = makeNaN();
    }
    else {
        const bool aNegInf = isNegInf(dA);
        const bool aPosInf = isPosInf(dA);
        const bool bNegInf = isNegInf(dB);
        const bool bPosInf = isPosInf(dB);

        const int code = (((aNegInf ? 1 : 0) << 3) |
                          ((aPosInf ? 1 : 0) << 2) |
                          ((bNegInf ? 1 : 0) << 1) |
                          ((bPosInf ? 1 : 0) << 0));

        switch ( code ) {
        case 6: /* +inf.0 + -inf.0 */
        case 9: /* -inf.0 + +inf.0 */
            expr = makeNaN();
            break;

        case 2: /* N + -inf.0 */
        case 8: /* -inf.0 + N */
        case 10: /* -inf.0 + -inf.0 */
            expr = makeNegInf();
            break;

        case 1: /* N + +inf.0 */
        case 4: /* +inf.0 + N */
        case 5: /* +inf.0 + +inf.0 */
            expr = makePosInf();
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
    ExtendedNumeric zero(makeInteger(0, true));
    return zero - a;
}

ExtendedNumeric
scam::operator-(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue dA = a.get();
    ScamValue dB = b.get();
    ScamValue expr { nullptr };

    if ( ! isSpecialNumeric(dA) && ! isSpecialNumeric(dB) ) {
        ExtendedNumeric minusOne(makeInteger(-1, true));
        ExtendedNumeric minusB = minusOne * b;
        return a + minusB;
    }
    else if ( isNaN(dA) || isNaN(dB) ) {
        expr = makeNaN();
    }
    else {
        const bool aNegInf = isNegInf(dA);
        const bool aPosInf = isPosInf(dA);
        const bool bNegInf = isNegInf(dB);
        const bool bPosInf = isPosInf(dB);

        const int code = (((aNegInf ? 1 : 0) << 3) |
                          ((aPosInf ? 1 : 0) << 2) |
                          ((bNegInf ? 1 : 0) << 1) |
                          ((bPosInf ? 1 : 0) << 0));

        switch ( code ) {
        case 5: /* +inf.0 - +inf.0 */
        case 10: /* -inf.0 - -inf.0 */
            expr = makeNaN();
            break;

        case 1: /* N - +inf.0 */
        case 8: /* -inf.0 - N */
        case 9: /* -inf.0 - +inf.0 */
            expr = makeNegInf();
            break;

        case 2: /* N - -inf.0 */
        case 4: /* +inf.0 - N */
        case 6: /* +inf.0 - -inf.0 */
            expr = makePosInf();
            break;

        default:
            throw
                ScamException("internal error in ExtendedNumeric subtraction");
            break;
        }
    }

    ExtendedNumeric tmp(NumericConverter::simplify(expr));
    return tmp;
}

ExtendedNumeric
scam::operator*(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue dA = a.get();
    ScamValue dB = b.get();
    ScamValue expr { nullptr };

    const auto joint = computeJoint(dA, dB);
    if ( ! joint.special ) {
        if ( joint.integer ) {
            const int rv = asInteger(dA) * asInteger(dB);
            expr = makeInteger(rv, joint.exact);
        }
        else if ( joint.rational ) {
            const RationalPair ratA = asRational(dA);
            const RationalPair ratB = asRational(dB);
            const int num = ratA.num * ratB.num;
            const int den = ratA.den * ratB.den;
            expr = makeRational(num, den, joint.exact);
        }
        else if ( joint.real ) {
            const double rv = asDouble(dA) * asDouble(dB);
            expr = makeReal(rv, joint.exact);
        }
        else {
            ExtendedNumeric rA(realPart(dA));
            ExtendedNumeric rB(realPart(dB));
            ExtendedNumeric iA(imagPart(dA));
            ExtendedNumeric iB(imagPart(dB));

            ExtendedNumeric rC = rA * rB - iA * iB;
            ExtendedNumeric iC = rA * iB + iA * rB;

            expr = makeComplex(rC.get(), iC.get());
        }
    }
    else if ( isNaN(dA) || isNaN(dB) ) {
        expr = makeNaN();
    }
    else {
        const bool aNegInf = isNegInf(dA);
        const bool aPosInf = isPosInf(dA);
        const bool bNegInf = isNegInf(dB);
        const bool bPosInf = isPosInf(dB);

        const int code = (((aNegInf ? 1 : 0) << 3) |
                          ((aPosInf ? 1 : 0) << 2) |
                          ((bNegInf ? 1 : 0) << 1) |
                          ((bPosInf ? 1 : 0) << 0));

        switch ( code ) {
        case 1: /* N * +inf.0 */
        case 2: /* N * -inf.0 */
            expr = signCheckRI(dA, dB);
            break;

        case 4: /* +inf.0 * N */
        case 8: /* -inf.0 * N */
            expr = signCheckRI(dB, dA);
            break;

        case 6: /* +inf.0 * -inf.0 */
        case 9: /* -inf.0 * +inf.0 */
            expr = makeNegInf();
            break;

        case 5: /* +inf.0 * +inf.0 */
        case 10: /* -inf.0 * -inf.0 */
            expr = makePosInf();
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
    ScamValue dA = a.get();
    ScamValue dB = b.get();
    ScamValue expr { nullptr };

    const auto joint = computeJoint(dA, dB);
    if ( ! joint.special ) {
        if ( joint.integer ) {
            const int iA = asInteger(dA);
            const int iB = asInteger(dB);
            if ( 0 == (iA % iB) ) {
                const int rv =  iA / iB;
                expr = makeInteger(rv, joint.exact);
            }
            else if ( ::abs(iA) <= 1e9 && ::abs(iB) <= 1e9 ) {
                expr = makeRational(iA, iB, joint.exact);
            }
            else {
                const double rv = (double) iA / (double) iB;
                expr = makeReal(rv, joint.exact);
            }
        }
        else if ( joint.rational ) {
            const RationalPair ratB = asRational(dB);
            int s = ratB.num < 0 ? -1 : 1;
            ScamValue recipricolB =
                makeRational(s * ratB.den, ::abs(ratB.num), joint.exact);
            ExtendedNumeric newB(recipricolB);
            expr = (a * newB).get();
        }
        else if ( joint.real ) {
            const double rv = asDouble(dA) / asDouble(dB);
            expr = makeReal(rv, joint.exact);
        }
        else {
            ExtendedNumeric rB(realPart(dB));
            ExtendedNumeric iB(imagPart(dB));
            ExtendedNumeric iBNeg = -iB;
            ExtendedNumeric bConj(makeComplex(rB.get(), iBNeg.get()));

            ExtendedNumeric num = a * bConj;
            ExtendedNumeric den = b * bConj;

            if ( isNaN(den.get() ) ) {
                expr = den.get();
            }
            else {
                ExtendedNumeric rNum(realPart(num.get()));
                ExtendedNumeric iNum(imagPart(num.get()));
                ExtendedNumeric rC = rNum / den;
                ExtendedNumeric iC = iNum / den;

                expr = makeComplex(rC.get(), iC.get());
            }
        }
    }
    else if ( isNaN(dA) || isNaN(dB) ) {
        expr = makeNaN();
    }
    else {
        const bool aNegInf = isNegInf(dA);
        const bool aPosInf = isPosInf(dA);
        const bool bNegInf = isNegInf(dB);
        const bool bPosInf = isPosInf(dB);

        if ( bNegInf || bPosInf ) {
            if ( aNegInf || aPosInf || (0 != asDouble(dA)) ) {
                expr = makeNaN();
            }
            else {
                const bool ex = isExact(dA);
                expr = makeInteger(0, ex);
            }
        }
        else {
            expr = signCheckRI(dB, dA);
        }
    }

    ExtendedNumeric tmp(NumericConverter::simplify(expr));
    return tmp;
}

ExtendedNumeric
scam::operator%(const ExtendedNumeric & a, const ExtendedNumeric & b)
{
    ScamValue dA  = a.get();
    ScamValue dB  = b.get();
    ScamValue expr { nullptr };

    const auto joint = computeJoint(dA, dB);
    if ( ! joint.special ) {

        if ( joint.integer ) {
            const int rv = asInteger(dA) % asInteger(dB);
            expr = makeInteger(rv, joint.exact);
        }
        else if ( joint.rational ) {
            ExtendedNumeric quotient = a / b;
            int q = (int) (0.0000001 + asDouble(quotient.get()));
            ExtendedNumeric x(makeInteger(q, joint.exact));
            ExtendedNumeric wp = x * b;
            return a - wp;
        }
        else {
            const double rv = fmod(asDouble(dA), asDouble(dB));
            expr = makeReal(rv, joint.exact);
        }
    }
    else {
        if ( ! isSpecialNumeric(dA) && 0 == asDouble(dA) ) {
            const bool ex = isExact(dA);
            expr = makeInteger(0, ex);
        }
        else {
            expr = makeNaN();
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
            const bool ex = isExact(r);
            return makeInteger(0, ex);
        }

        if ( rVal > 0 ) {
            return i;
        }

        if ( isPosInf(i) ) {
            return makeNegInf();
        }

        return makePosInf();
    }

    JointValues computeJoint(ScamValue lhs, ScamValue rhs)
    {
        return JointValues {
            isSpecialNumeric(lhs) || isSpecialNumeric(rhs),
            isComplex(lhs) && isComplex(rhs),
            isReal(lhs) && isReal(rhs),
            isRational(lhs) && isRational(rhs),
            isInteger(lhs) && isInteger(rhs),
            isExact(lhs) && isExact(rhs)
        };
    }
}
