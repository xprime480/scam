#include "expr/EqualityOps.hpp"

#include "expr/ScamExpr.hpp"
#include "expr/ScamNumeric.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"

#include <cmath>

using namespace scam;

namespace
{
    extern bool equalsNumeric(ScamValue lhs, ScamValue rhs);
    extern bool equalsByteVector(ScamValue lhs, ScamValue rhs);
    extern bool equalsDict(ScamValue lhs, ScamValue rhs);
    extern bool equalsVector(ScamValue lhs, ScamValue rhs);
}


bool scam::equals(ScamValue lhs, ScamValue rhs)
{
    bool rv = false;

    if ( isNumeric(lhs) && isNumeric(rhs) ) {
        rv = equalsNumeric(lhs, rhs);
    }

    else if ( lhs->type != rhs->type ) {
        rv = false;
    }

    else if ( isBoolean(lhs) ) {
        rv = (BOOLVAL(lhs) == BOOLVAL(rhs));
    }

    else if ( isByteVector(lhs) ) {
        rv = equalsByteVector(lhs, rhs);
    }

    else if ( isChar(lhs) ) {
        rv = (CHARVAL(lhs) == CHARVAL(rhs));
    }

    else if ( isCons(lhs) ) {
        rv = (equals(CAR(lhs), CAR(rhs)) && equals(CDR(lhs), CDR(rhs)));
    }

    else if ( isDict(lhs) ) {
        rv = equalsDict(lhs, rhs);
    }

    else if ( error(lhs) ) {
        rv = (STRVAL(lhs) == STRVAL(rhs));
    }

    else if ( isKeyword(lhs) ) {
        rv = (STRVAL(lhs) == STRVAL(rhs));
    }

    else if ( isNil(lhs) ) {
        rv = true;
    }

    else if ( isNull(lhs) ) {
        rv = false;
    }

    else if ( isString(lhs) ) {
        rv = (STRVAL(lhs) == STRVAL(rhs));
    }

    else if ( isSymbol(lhs) ) {
        rv = (STRVAL(lhs) == STRVAL(rhs));
    }

    else if ( isVector(lhs) ) {
        rv = equalsVector(lhs, rhs);
    }

    else {
        rv = (lhs == rhs);    // default case
    }

    return rv;
}

namespace
{
    bool equalsNumeric(ScamValue lhs, ScamValue rhs)
    {
        if ( isNaN(lhs) || isNaN(rhs) ) {
            return isNaN(lhs) && isNaN(rhs);
        }
        if ( isNegInf(lhs) || isNegInf(rhs) ) {
            return isNegInf(lhs) && isNegInf(rhs);
        }
        if ( isPosInf(lhs) || isPosInf(rhs) ) {
            return isPosInf(lhs) && isPosInf(rhs);
        }

        ScamValue lhsV = const_cast<ScamValue>(dynamic_cast<ConstScamValue>(lhs));
        ScamValue thatV = const_cast<ScamValue>(rhs);
        ScamValue lhsH = realPart(lhsV);
        ScamValue thatH = realPart(thatV);

        const double lhsR = asDouble(lhsH);
        const double thatR = asDouble(thatH);

        if ( ::fabs(lhsR- thatR) > 1e-9 ) {
            return false;
        }

        if ( isPureComplex(lhs) || isPureComplex(rhs) ) {
            const double lhsI = asDouble(imagPart(lhsV));
            const double thatI = asDouble(imagPart(thatV));
            if ( ::fabs(lhsI- thatI) > 1e-9 ) {
                return false;
            }
        }

        return true;
    }

    bool equalsByteVector(ScamValue lhs, ScamValue rhs)
    {
        if ( BYTEVECTOR(lhs).size() != BYTEVECTOR(rhs).size() ) {
            return false;
        }

        for ( size_t idx = 0 ; idx < BYTEVECTOR(rhs).size() ; ++idx ) {
            if ( BYTEVECTOR(lhs)[idx] != BYTEVECTOR(rhs)[idx] ) {
                return false;
            }
        }

        return true;
    }

    bool equalsDict(ScamValue lhs, ScamValue rhs)
    {
        if ( DICTKEYS(lhs).size() != DICTKEYS(rhs).size() ) {
            return false;
        }

        ScamValue hack = const_cast<ScamValue>(dynamic_cast<ConstScamValue>(lhs));
        size_t len = length(hack);
        size_t otherIdx = len+1;

        for ( size_t lhsIdx = 0 ; lhsIdx < len ; ++lhsIdx ) {
            ScamValue myKey = DICTKEYS(lhs)[lhsIdx];
            for ( otherIdx = 0 ; otherIdx < len ; ++otherIdx ) {
                if ( equals(DICTKEYS(rhs)[otherIdx], myKey) ) {
                    ScamValue myVal = DICTVALS(lhs)[lhsIdx];
                    if ( ! equals(DICTVALS(rhs)[otherIdx], myVal) ) {
                        return false;
                    }
                    break;
                }
            }
            if ( otherIdx >= len ) {
                return false;
            }
        }

        return true;
    }

    bool equalsVector(ScamValue lhs, ScamValue rhs)
    {
        if ( VECTOR(lhs).size() != VECTOR(rhs).size() ) {
            return false;
        }

        for ( size_t idx = 0 ; idx < VECTOR(lhs).size() ; ++idx ) {
            if ( ! equals(VECTOR(lhs)[idx], VECTOR(rhs)[idx]) ) {
                return false;
            }
        }

        return true;
    }
}


