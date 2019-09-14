#include "prim/MathOps.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "util/ArgListHelper.hpp"
#include "util/Parameter.hpp"
#include "value/ValueFactory.hpp"

#include <algorithm>
#include <numeric>
#include <sstream>

using namespace scam;
using namespace std;

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
