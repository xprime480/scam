#include "expr/EqualityOps.hpp"

#include "expr/SequenceOps.hpp"
#include "value/ScamData.hpp"
#include "value/ScamNumeric.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"

#include <cmath>

using namespace scam;
using namespace std;

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
        rv = (lhs->boolValue() == rhs->boolValue());
    }

    else if ( isByteVector(lhs) ) {
        rv = equalsByteVector(lhs, rhs);
    }

    else if ( isChar(lhs) ) {
        rv = (lhs->charValue() == rhs->charValue());
    }

    else if ( isPair(lhs) ) {
        rv = (equals(lhs->carValue(), rhs->carValue()) &&
              equals(lhs->cdrValue(), rhs->cdrValue()));
    }

    else if ( isDict(lhs) ) {
        rv = equalsDict(lhs, rhs);
    }

    else if ( isKeyword(lhs) || isString(lhs) || isSymbol(lhs) ) {
        rv = (lhs->stringValue() == rhs->stringValue());
    }

    else if ( isNull(lhs) || isEof(lhs) ) {
        rv = true;
    }

    else if ( isNothing(lhs) ) {
        rv = false;
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

        ScamValue lhsH = realPart(lhs);
        ScamValue thatH = realPart(rhs);
        const double lhsR = asDouble(lhsH);
        const double thatR = asDouble(thatH);

        if ( ::fabs(lhsR- thatR) > 1e-9 ) {
            return false;
        }

        if ( isPureComplex(lhs) || isPureComplex(rhs) ) {
            const double lhsI = asDouble(imagPart(lhs));
            const double thatI = asDouble(imagPart(rhs));
            if ( ::fabs(lhsI- thatI) > 1e-9 ) {
                return false;
            }
        }

        return true;
    }

    bool equalsByteVector(ScamValue lhs, ScamValue rhs)
    {
        const vector<unsigned char> & lhbv = lhs->byteVectorData();
        const vector<unsigned char> & rhbv = rhs->byteVectorData();

        if ( lhbv.size() != rhbv.size() ) {
            return false;
        }

        size_t size = lhbv.size();

        for ( size_t idx = 0 ; idx < size ; ++idx ) {
            if ( lhbv[idx] != rhbv[idx] ) {
                return false;
            }
        }

        return true;
    }

    bool equalsDict(ScamValue lhs, ScamValue rhs)
    {
        const vector<ScamValue> & lhk = lhs->dictKeys();
        const vector<ScamValue> & rhk = rhs->dictKeys();

        if ( lhk.size() != rhk.size() ) {
            return false;
        }

        size_t len = lhk.size();
        size_t otherIdx = len+1;
        const vector<ScamValue> & lhv = lhs->dictValues();
        const vector<ScamValue> & rhv = rhs->dictValues();

        for ( size_t lhsIdx = 0 ; lhsIdx < len ; ++lhsIdx ) {
            ScamValue myKey = lhk[lhsIdx];
            for ( otherIdx = 0 ; otherIdx < len ; ++otherIdx ) {
                if ( equals(rhk[otherIdx], myKey) ) {
                    ScamValue myVal = lhv[lhsIdx];
                    if ( ! equals(rhv[otherIdx], myVal) ) {
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
        const vector<ScamValue> & lhv = lhs->vectorData();
        const vector<ScamValue> & rhv = rhs->vectorData();

        if ( lhv.size() != rhv.size() ) {
            return false;
        }

        size_t size = lhv.size();

        for ( size_t idx = 0 ; idx < size ; ++idx ) {
            if ( ! equals(lhv[idx], rhv[idx]) ) {
                return false;
            }
        }

        return true;
    }
}
