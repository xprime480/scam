#include "prim/EquivOps.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/Parameter.hpp"

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

    extern bool doEqv(ScamValue obj1, ScamValue obj2);
    extern bool doEqual(ScamValue obj1, ScamValue obj2);
}

void scam::applyEqP(ScamValue args,
                    Continuation * cont,
                    ScamEngine * engine)
{
    ObjectParameter p0, p1;
    if ( argsToParms(args, engine, "eq?", p0, p1) ) {
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

void scam::applyEqvP(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    ObjectParameter p0, p1;
    if ( argsToParms(args, engine, "eqv?", p0, p1) ) {
        ScamValue obj1 = p0.value;
        ScamValue obj2 = p1.value;

        bool match = doEqv(obj1, obj2);
        ScamValue rv = makeBoolean(match);
        cont->handleValue(rv);
    }
}

void scam::applyEqualP(ScamValue args,
                       Continuation * cont,
                       ScamEngine * engine)
{
    ObjectParameter p0, p1;
    if ( argsToParms(args, engine, "equal?", p0, p1) ) {
        ScamValue obj1 = p0.value;
        ScamValue obj2 = p1.value;

        bool match = doEqual(obj1, obj2);
        ScamValue rv = makeBoolean(match);
        cont->handleValue(rv);
    }
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
        const ScamData::DictKeyData & k1 = obj1->dictKeys();
        const ScamData::DictKeyData & k2 = obj2->dictKeys();
        const ScamData::DictValueData & v1 = obj1->dictValues();
        const ScamData::DictValueData & v2 = obj2->dictValues();

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

    bool doEqv(ScamValue obj1, ScamValue obj2)
    {
        bool rv = false;

        if ( isNumeric(obj1) && isNumeric(obj2) ) {
            if ( isExact(obj1) == isExact(obj2) ) {
                rv = ExtendedNumeric(obj1) == ExtendedNumeric(obj2);
            }
        }
        else if ( obj1->type == obj2->type ) {
            switch ( obj1->type ) {
            case ScamData::Nothing:
                break;

            case ScamData::Null:
                rv = true;
                break;

            case ScamData::Boolean:
                rv = compareBoolean(obj1, obj2);
                break;

            case ScamData::Character:
                rv = compareChar(obj1, obj2);
                break;

            case ScamData::Symbol:
            case ScamData::Keyword:
                rv = compareStringLike(obj1, obj2);
                break;

            case ScamData::String:
            case ScamData::Pair:
            case ScamData::Vector:
            case ScamData::ByteVector:
            case ScamData::Dict:
                rv = obj1 == obj2;
                break;

            case ScamData::Error:
            case ScamData::Closure:
            case ScamData::Class:
            case ScamData::Instance:
            case ScamData::Cont:
            case ScamData::Primitive:
            case ScamData::SpecialForm:
            case ScamData::Port:
            default:
                rv = obj1 == obj2;
                break;
            }
        }

        return rv;
    }

    bool doEqual(ScamValue obj1, ScamValue obj2)
    {
        bool rv = false;

        if ( isNumeric(obj1) && isNumeric(obj2) ) {
            rv = doEqv(obj1, obj2);
        }
        else if ( obj1->type == obj2->type ) {
            switch ( obj1->type ) {
            case ScamData::String:
                rv = compareStringLike(obj1, obj2);
                break;

            case ScamData::Pair:
                rv = comparePair(obj1, obj2);
                break;

            case ScamData::Vector:
            case ScamData::ByteVector:
                rv = compareSequence(obj1, obj2);
                break;

            case ScamData::Dict:
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
