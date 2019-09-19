#include "prim/MathOps.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "util/ArgListHelper.hpp"
#include "util/Parameter.hpp"
#include "value/ScamNumeric.hpp"
#include "value/ValueFactory.hpp"

#include <algorithm>
#include <cmath>
#include <numeric>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern ExtendedNumeric
    do_add(vector<ExtendedNumeric> const & ns, ScamValue & state);

    extern ExtendedNumeric
    do_sub(vector<ExtendedNumeric> const & ns, ScamValue & state);

    extern ExtendedNumeric
    do_mul(vector<ExtendedNumeric> const & ns, ScamValue & state);

    extern ExtendedNumeric
    do_div(vector<ExtendedNumeric> const & ns, ScamValue & state);

    extern ExtendedNumeric
    do_mod(vector<ExtendedNumeric> const & ns, ScamValue & state);

    extern ScamValue convertWith(ScamValue initial, double (fn)(double value));
}

void scam::applyCeiling(ScamValue args, Continuation * cont)
{
    static const char * name = "ceiling";

    FiniteRealParameter p0;
    if ( argsToParms(args, name, p0) ) {
        ScamValue rv = convertWith(p0.value, ceil);
        cont->handleValue(rv);
    }
}

void scam::applyFloor(ScamValue args, Continuation * cont)
{
    static const char * name = "floor";

    FiniteRealParameter p0;
    if ( argsToParms(args, name, p0) ) {
        ScamValue rv = convertWith(p0.value, floor);
        cont->handleValue(rv);
    }
}

void scam::applyImagPart(ScamValue args, Continuation * cont)
{
    static const char * name = "imag-part";

    ComplexParameter p0;
    if ( argsToParms(args, name, p0) ) {
        ScamValue rv = imagPart(p0.value);
        cont->handleValue(rv);
    }
}

void scam::applyRealPart(ScamValue args, Continuation * cont)
{
    static const char * name = "real-part";

    ComplexParameter p0;
    if ( argsToParms(args, name, p0) ) {
        ScamValue rv = realPart(p0.value);
        cont->handleValue(rv);
    }
}

void scam::applyRound(ScamValue args, Continuation * cont)
{
    static const char * name = "round";

    FiniteRealParameter p0;
    if ( argsToParms(args, name, p0) ) {
        ScamValue rv = convertWith(p0.value, round);
        cont->handleValue(rv);
    }
}

void scam::applySqrt(ScamValue args, Continuation * cont)
{
    static const char * name = "sqrt";

    NumericParameter p0;
    if ( argsToParms(args, name, p0) ) {
        ScamValue rv = makeNothing();
        ScamValue value = p0.value;

        if ( isSpecialNumeric(value) ) {
            if ( isNegInf(value) ) {
                rv = makeComplex(makeInteger(0, true), makeNegInf());
            }
            else {
                rv = value;
            }
        }
        else if ( isReal(value) ) {
            double d = asDouble(value);
            double r = sqrt(abs(d));
            if ( d < 0 ) {
                rv = makeComplex(makeInteger(0, true),
                                 makeReal(r, false));
            }
            else {
                rv = makeReal(r, false);
            }
        }
        else {
            double real   = asDouble(realPart(value));
            double imag   = asDouble(imagPart(value));
            double r      = sqrt(real*real + imag*imag);
            double theta  = atan2(imag, real);
            double rr     = sqrt(r);
            double rtheta = theta / 2;
            double rreal  = rr * cos(rtheta);
            double rimag  = rr * sin(rtheta);

            rv = makeComplex(makeReal(rreal, false),
                             makeReal(rimag, false));
        }

        cont->handleValue(rv);
    }
}

void scam::applyTruncate(ScamValue args, Continuation * cont)
{
    static const char * name = "truncate";

    FiniteRealParameter p0;
    if ( argsToParms(args, name, p0) ) {
        ScamValue rv = convertWith(p0.value, trunc);
        cont->handleValue(rv);
    }
}

#define MATH_OP_DEFINE(Name, Proc) \
    void scam::apply##Name(ScamValue args, Continuation * cont)           \
        {                                                                 \
            static const char * context { #Name };                        \
            NumericParameter pNum;                                        \
            CountedParameter p0(pNum);                                    \
            if ( argsToParms(args, context, p0) ) {                       \
                ScamValue rv = numericAlgorithm(p0.value, context, Proc); \
                if ( isUnhandledError(rv) ) {                             \
                    ScamEngine::getEngine().handleError(rv);              \
                }                                                         \
                else {                                                    \
                    cont->handleValue(rv);                                \
                }                                                         \
            }                                                             \
        }


MATH_OP_DEFINE(Add, do_add);
MATH_OP_DEFINE(Sub, do_sub);
MATH_OP_DEFINE(Mul, do_mul);
MATH_OP_DEFINE(Div, do_div);
MATH_OP_DEFINE(Mod, do_mod);

#undef MATH_OP_DEFINE


namespace
{
    ExtendedNumeric
    do_add(vector<ExtendedNumeric> const & ns, ScamValue & state)
    {
        ExtendedNumeric zero(makeInteger(0, true));
        return accumulate(ns.begin(), ns.end(), zero);
    }

    ExtendedNumeric
    do_sub(vector<ExtendedNumeric> const & ns, ScamValue & state)
    {
        ExtendedNumeric zero(makeInteger(0, true));
        ExtendedNumeric total = zero;

        switch ( ns.size() ) {
        case 0:
            break;

        case 1:
            total = - ns[0];
            break;

        default:
            total = ns[0] - accumulate(++(ns.begin()), ns.end(), zero);
            break;
        }
        return total;
    }

    ExtendedNumeric
    do_mul(vector<ExtendedNumeric> const & ns, ScamValue & state)
    {
        ExtendedNumeric one(makeInteger(1, true));
        return accumulate(ns.begin(), ns.end(),
                          one,
                          multiplies<ExtendedNumeric>());
    }

    ExtendedNumeric
    do_div(vector<ExtendedNumeric> const & ns, ScamValue & state)
    {
        ExtendedNumeric zero(makeInteger(0, true));
        ExtendedNumeric one(makeInteger(1, true));

        state = makeBoolean(false);
        if ( ns.empty() ) {
            return one;
        }
        else {
            auto iter = ns.begin();
            if ( ns.size() > 1 ) {
                iter++;
            }
            if ( ns.end() != find(iter, ns.end(), zero) ) {
                state = makeError("Division By Zero");
                state->errorCategory() = argsCategory;
                return zero;
            }
        }

        ExtendedNumeric total = one;
        switch ( ns.size() ) {
        case 0:
            break;

        case 1:
            total = total / ns[0];
            break;

        default:
            total = ns[0];
            total = total / accumulate(++(ns.begin()), ns.end(),
                                       one,
                                       multiplies<ExtendedNumeric>());
            break;
        }
        return total;
    }

    ExtendedNumeric
    do_mod(vector<ExtendedNumeric> const & ns, ScamValue & state)
    {
        ExtendedNumeric zero(makeInteger(0, true));

        state = makeBoolean(false);
        if ( ns.size() < 2 ) {
            return zero;
        }
        if ( zero == ns[1] ) {
            state = makeError("Modulus By Zero");
            state->errorCategory() = argsCategory;
            return zero;
        }

        return ns[0] % ns[1];
    }

    ScamValue convertWith(ScamValue initial, double (fn)(double value))
    {
        ScamValue rv = initial;

        if ( ! isInteger(rv) ) {
            bool exact = isExact(rv);
            double d = fn(asDouble(rv));
            rv = makeInteger((int) d, exact);
        }

        return rv;
    }

}
