#include "prim/EquivOps.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "input/ListParser.hpp"
#include "util/ArgListHelper.hpp"

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
    static const char * name = "eq?";
    ScamValue obj1, obj2;
    if ( ! getTwoObjs(args, cont, engine, name, obj1, obj2) ) {
        return;
    }

    try {
        bool rv = false;
        if ( isNumeric(obj1) && isNumeric(obj2) ) {
            rv = (obj1 == obj2) && (! isNaN(obj1));
        }
        else {
            rv = doEqv(obj1, obj2);
        }
        cont->handleValue(makeBoolean(rv));
    }
    catch ( int ) {
        ScamValue err = makeError("eq? not implemented for this type");
        engine->handleError(err);
    }
}

void scam::applyEqvP(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * name = "eqv?";
    ScamValue obj1, obj2;
    if ( ! getTwoObjs(args, cont, engine, name, obj1, obj2) ) {
        return;
    }

    try {
        bool rv = doEqv(obj1, obj2);
        cont->handleValue(makeBoolean(rv));
    }
    catch ( int ) {
        ScamValue err = makeError("eqv? not implemented for this type");
        engine->handleError(err);
    }
}

void scam::applyEqualP(ScamValue args,
                       Continuation * cont,
                       ScamEngine * engine)
{
    static const char * name = "equal?";
    ScamValue obj1, obj2;
    if ( ! getTwoObjs(args, cont, engine, name, obj1, obj2) ) {
        return;
    }

    try {
        bool rv = doEqual(obj1, obj2);
        cont->handleValue(makeBoolean(rv));
    }
    catch ( int ) {
        ScamValue err = makeError("equal? not implemented for this type");
        engine->handleError(err);
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
            case ScamData::Error:
                rv = compareStringLike(obj1, obj2);
                break;

            case ScamData::String:
            case ScamData::Pair:
            case ScamData::Vector:
            case ScamData::ByteVector:
            case ScamData::Dict:
                rv = obj1 == obj2;
                break;

            case ScamData::Closure:
            case ScamData::Class:
            case ScamData::Instance:
            case ScamData::Cont:
            case ScamData::Primitive:
            case ScamData::SpecialForm:
            case ScamData::Port:
                rv = obj1 == obj2;
                break;

            default:
                throw 0;
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
