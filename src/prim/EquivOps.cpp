#include "prim/EquivOps.hpp"

#include "Continuation.hpp"
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
    if ( ! getTwoObjs(args, cont, name, obj1, obj2) ) {
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
        cont->run(makeBoolean(rv));
    }
    catch ( int ) {
        cont->run(makeError("eq? not implemented for this type"));
        return;
    }
}

void scam::applyEqvP(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * name = "eqv?";
    ScamValue obj1, obj2;
    if ( ! getTwoObjs(args, cont, name, obj1, obj2) ) {
        return;
    }

    try {
        bool rv = doEqv(obj1, obj2);
        cont->run(makeBoolean(rv));
    }
    catch ( int ) {
        cont->run(makeError("eqv? not implemented for this type"));
        return;
    }
}

void scam::applyEqualP(ScamValue args,
                       Continuation * cont,
                       ScamEngine * engine)
{
    static const char * name = "equal?";
    ScamValue obj1, obj2;
    if ( ! getTwoObjs(args, cont, name, obj1, obj2) ) {
        return;
    }

    try {
        bool rv = doEqual(obj1, obj2);
        cont->run(makeBoolean(rv));
    }
    catch ( int ) {
        cont->run(makeError("equal? not implemented for this type"));
        return;
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
        return STRVAL(obj1) == STRVAL(obj2);
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
        int len = length(obj1);

        for ( int idx1 = 0 ; idx1 < len ; ++idx1 ) {
            ScamValue myKey = DICTKEYS(obj1)[idx1];
            bool keyExists = false;
            for ( int idx2 = 0 ; (! keyExists) && (idx2 < len) ; ++idx2 ) {
                if ( doEqual(DICTKEYS(obj2)[idx2], myKey) ) {
                    keyExists = true;
                    ScamValue myVal = DICTVALS(obj1)[idx1];
                    if ( ! doEqual(DICTVALS(obj2)[idx2], myVal) ) {
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
