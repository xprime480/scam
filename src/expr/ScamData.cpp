#include "expr/ScamData.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamNumeric.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "input/ClassDefParser.hpp"
#include "input/LambdaParser.hpp"

using namespace scam;
using namespace std;

ScamData::ScamData(unsigned long type, bool managed)
    : ManagedObject(managed)
    , type(type)
    , metadata(nullptr)
{
    switch ( type ) {
    case ScamData::Error:
    case ScamData::Keyword:
    case ScamData::String:
    case ScamData::Symbol:
        STRVALP(this) = new string;
        break;

    case ScamData::ByteVector:
        BYTEVECTORP(this) = new vector<unsigned char>;

        break;

    case ScamData::Dict:
        DICTKEYSP(this) = new vector<ScamValue>;
        DICTVALSP(this) = new vector<ScamValue>;
        break;

    case ScamData::Vector:
        VECTORP(this) = new vector<ScamValue>;
        break;

    case ScamData::SpecialForm:
        SFNAMEP(this) = new string;
        SFFUNCP(this) = new SfFunction;
        break;

    case ScamData::Primitive:
        PRIMNAMEP(this) = new string;
        PRIMFUNCP(this) = new PrimFunction;
        break;

    default:
        break;
    }
}

ScamData::~ScamData()
{
    switch ( type ) {
    case ScamData::Error:
    case ScamData::Keyword:
    case ScamData::String:
    case ScamData::Symbol:
        delete STRVALP(this);
        break;

    case ScamData::ByteVector:
        delete BYTEVECTORP(this);

        break;

    case ScamData::Dict:
        delete DICTKEYSP(this);
        delete DICTVALSP(this);
        break;

    case ScamData::Vector:
        delete VECTORP(this);
        break;

    case ScamData::SpecialForm:
        delete SFNAMEP(this);
        delete SFFUNCP(this);
        break;

    case ScamData::Primitive:
        delete PRIMNAMEP(this);
        delete PRIMFUNCP(this);
        break;

    default:
        break;
    }
}

ScamData * ScamData::makeInstance(unsigned long type, bool managed)
{
    return new ScamData(type, managed);
}

void ScamData::mark() const
{
    if ( isMarked() ) {
        return;
    }

    ManagedObject::mark();
    if ( metadata ) {
        metadata->mark();
    }

    ScamValue hack = const_cast<ScamValue>(this);
    if ( isNumeric(hack) ) {
        if ( isPureComplex(hack) ) {
            REALPART(this)->mark();
            IMAGPART(this)->mark();
        }
        return;
    }

    switch ( type ) {
    case ScamData::Class:
        CLASSDEF(this)->mark();
        CLASSENV(this)->mark();
        break;

    case ScamData::Closure:
        CLOSUREDEF(this)->mark();
        CLOSUREENV(this)->mark();
        break;

    case ScamData::Cons:
        CAR(this)->mark();
        CDR(this)->mark();
        break;

    case ScamData::Cont:
        CONTINUATION(this)->mark();
        break;

    case ScamData::Dict:
        for ( size_t idx = 0 ; idx < DICTKEYS(this).size() ; ++idx ) {
            DICTKEYS(this)[idx]->mark();
            DICTVALS(this)[idx]->mark();
        }
        break;

    case ScamData::Instance:
        INSTANCELOCALENV(this)->mark();
        INSTANCEPRIVENV(this)->mark();
        break;

    case ScamData::Vector:
        for ( auto const & e : VECTOR(this) ) {
            e->mark();
        }
        break;

    default:
        break;
    }
}

void ScamData::setMeta(string const & key, ScamValue value) const
{
    if ( ! metadata ) {
        metadata = standardMemoryManager.make<Env>();
    }

    ScamValue k = makeSymbol(key);

    if ( metadata->check(k) ) {
        metadata->assign(k, value);
    }
    else {
        metadata->put(k, value);
    }
}

bool ScamData::hasMeta(string const & key) const
{
    if ( ! metadata ) {
        return false;
    }

    ScamValue k = makeSymbol(key);
    return metadata->check(k);
}

ScamValue ScamData::getMeta(string const & key) const
{
    ScamValue rv = makeNil();
    if ( ! metadata ) {
        return rv;
    }

    ScamValue k  = makeSymbol(key);
    if ( metadata->check(k) ) {
        rv = metadata->get(k);
    }

    return rv;
}
