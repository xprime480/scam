#include "prim/EquivOps.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamException.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "expr/SequenceOps.hpp"
#include "util/Parameter.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern bool compareBoolean(ScamValue obj1, ScamValue obj2);
    extern bool compareChar(ScamValue obj1, ScamValue obj2);
    extern bool compareStringLike(ScamValue obj1, ScamValue obj2);
    extern bool comparePair(ScamValue obj1, ScamValue obj2);
    extern bool compareSequence(ScamValue obj1, ScamValue obj2);
    extern bool compareDict(ScamValue obj1, ScamValue obj2);

    extern bool doEqual(ScamValue obj1, ScamValue obj2);
}

void scam::applyEqP(ScamValue args, Continuation * cont)
{
    ObjectParameter p0, p1;
    if ( argsToParms(args, "eq?", p0, p1) ) {
        ScamValue obj1 = p0.value;
        ScamValue obj2 = p1.value;
        ScamValue rv = makeNothing();

        bool match = false;
        if ( isNumeric(obj1) && isNumeric(obj2) ) {
            match = (obj1 == obj2) && (! isNaN(obj1));
        }
        else {
            match = doEqv(obj1, obj2);
        }

        rv = makeBoolean(match);
        cont->handleValue(rv);
    }
}

void scam::applyEqvP(ScamValue args, Continuation * cont)
{
    ObjectParameter p0, p1;
    if ( argsToParms(args, "eqv?", p0, p1) ) {
        ScamValue obj1 = p0.value;
        ScamValue obj2 = p1.value;

        bool match = doEqv(obj1, obj2);
        ScamValue rv = makeBoolean(match);
        cont->handleValue(rv);
    }
}

void scam::applyEqualP(ScamValue args, Continuation * cont)
{
    ObjectParameter p0, p1;
    if ( argsToParms(args, "equal?", p0, p1) ) {
        ScamValue obj1 = p0.value;
        ScamValue obj2 = p1.value;

        bool match = doEqual(obj1, obj2);
        ScamValue rv = makeBoolean(match);
        cont->handleValue(rv);
    }
}

bool scam::doEqv(ScamValue obj1, ScamValue obj2)
{
    bool rv = false;

    if ( isNumeric(obj1) && isNumeric(obj2) ) {
        if ( isExact(obj1) == isExact(obj2) ) {
            rv = ExtendedNumeric(obj1) == ExtendedNumeric(obj2);
        }
    }
    else if ( obj1->type == obj2->type ) {
        switch ( obj1->type ) {
        case ScamValueType::Nothing:
            break;

        case ScamValueType::Null:
            rv = true;
            break;

        case ScamValueType::Boolean:
            rv = compareBoolean(obj1, obj2);
            break;

        case ScamValueType::Character:
            rv = compareChar(obj1, obj2);
            break;

        case ScamValueType::Symbol:
        case ScamValueType::Keyword:
            rv = compareStringLike(obj1, obj2);
            break;

        case ScamValueType::String:
        case ScamValueType::Pair:
        case ScamValueType::Vector:
        case ScamValueType::ByteVector:
        case ScamValueType::Dict:
            rv = obj1 == obj2;
            break;

        case ScamValueType::Error:
        case ScamValueType::Closure:
        case ScamValueType::Class:
        case ScamValueType::Instance:
        case ScamValueType::Cont:
        case ScamValueType::Primitive:
        case ScamValueType::SpecialForm:
        case ScamValueType::Port:
        default:
            rv = obj1 == obj2;
            break;
        }
    }

    return rv;
}

namespace
{
    bool compareBoolean(ScamValue obj1, ScamValue obj2)
    {
        return asBool(obj1) == asBool(obj2);
    }

    bool compareChar(ScamValue obj1, ScamValue obj2)
    {
        return asChar(obj1) == asChar(obj2);
    }

    bool compareStringLike(ScamValue obj1, ScamValue obj2)
    {
        return obj1->stringValue() == obj2->stringValue();
    }

    bool comparePair(ScamValue obj1, ScamValue obj2)
    {
        return (doEqual(getCar(obj1), getCar(obj2)) &&
                doEqual(getCdr(obj1), getCdr(obj2)));
    }

    bool compareSequence(ScamValue obj1, ScamValue obj2)
    {
        bool rv = true;

        int len1 = length(obj1);
        int len2 = length(obj2);

        if ( len1 == len2 ) {
            for ( int idx = 0 ; rv && (idx < len1) ; ++idx ) {
                rv = doEqual(nthcar(obj1, idx), nthcar(obj2, idx));
            }
        }

        return rv;
    }

    bool compareDict(ScamValue obj1, ScamValue obj2)
    {
        const vector<ScamValue> & k1 = obj1->dictKeys();
        const vector<ScamValue> & k2 = obj2->dictKeys();
        const vector<ScamValue> & v1 = obj1->dictValues();
        const vector<ScamValue> & v2 = obj2->dictValues();

        int len = length(obj1);

        for ( int idx1 = 0 ; idx1 < len ; ++idx1 ) {
            ScamValue myKey = k1[idx1];
            bool keyExists = false;
            for ( int idx2 = 0 ; (! keyExists) && (idx2 < len) ; ++idx2 ) {
                if ( doEqual(k2[idx2], myKey) ) {
                    keyExists = true;
                    ScamValue myVal = v1[idx1];
                    if ( ! doEqual(v2[idx2], myVal) ) {
                        return false;
                    }
                }
            }
            if ( ! keyExists ) {
                return false;
            }
        }

        return true;
    }

    bool doEqual(ScamValue obj1, ScamValue obj2)
    {
        bool rv = false;

        if ( isNumeric(obj1) && isNumeric(obj2) ) {
            rv = doEqv(obj1, obj2);
        }
        else if ( obj1->type == obj2->type ) {
            switch ( obj1->type ) {
            case ScamValueType::String:
                rv = compareStringLike(obj1, obj2);
                break;

            case ScamValueType::Pair:
                rv = comparePair(obj1, obj2);
                break;

            case ScamValueType::Vector:
            case ScamValueType::ByteVector:
                rv = compareSequence(obj1, obj2);
                break;

            case ScamValueType::Dict:
                rv = compareDict(obj1, obj2);
                break;

            default:
                rv = doEqv(obj1, obj2);
                break;
            }
        }

        return rv;
    }
}
