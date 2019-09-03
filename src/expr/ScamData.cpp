#include "expr/ScamData.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "env/Env.hpp"
#include "expr/ScamNumeric.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "form/SyntaxRules.hpp"
#include "port/ScamPort.hpp"
#include "util/ClassDef.hpp"
#include "util/LambdaDef.hpp"

using namespace scam;
using namespace std;

ScamData::ScamData(DataTagType type, bool managed)
    : ManagedObject(managed)
    , type(type)
    , immutable(true)
    , metadata(nullptr)
{
    if ( 0 != (type & Numeric) ) {
        value.numericValue = new NumericData;
        switch ( type ) {
        case Complex:
            value.numericValue->value.complexValue = new ComplexData;
            break;

        case Rational:
            value.numericValue->value.rationalValue = new RationalData;
            break;

        default:
            /* nothing more to be done */
            break;
        }

        return;
    }

    switch ( type ) {

    case ScamData::String:
        immutable = false;
        /* fallthrough */

    case ScamData::Keyword:
    case ScamData::Symbol:
        value.strVal = new string;
        break;

    case ScamData::Error:
        value.errorData = new ErrorData;
        value.errorData->category = new ScamData(ScamData::Nothing);
        break;

    case ScamData::Pair:
        value.pairValue = new PairData;
        immutable = false;
        break;

    case ScamData::Vector:
        value.vectorData = new VectorData;
        break;

    case ScamData::ByteVector:
        value.byteVectorData = new ByteVectorData;
        break;

    case ScamData::Dict:
        value.dictData = new DictData;
        break;

    case ScamData::Closure:
        value.closureData = new ClosureData;
        break;

    case ScamData::Class:
        value.classValue = new ClassData;
        break;

    case ScamData::Instance:
        value.instanceData = new InstanceData;
        break;

    case ScamData::Primitive:
        value.primitiveData = new PrimitiveData;
        break;

    case ScamData::SpecialForm:
        value.specialFormData = new SpecialFormData;
        break;

    case ScamData::Port:
        value.portData = nullptr;
        break;

    case ScamData::Syntax:
        value.syntaxData = new SyntaxRules;
        break;

    case ScamData::ScamEnv:
        value.envData = nullptr;
        break;

    default:
        break;
    }
}

ScamData::~ScamData()
{
    if ( 0 != (type & Numeric) ) {
        switch ( type ) {
        case Complex:
            delete value.numericValue->value.complexValue;
            break;

        case Rational:
            delete value.numericValue->value.rationalValue;
            break;

        default:
            /* nothing more to be done */
            break;
        }

        delete value.numericValue;
        return;
    }

    switch ( type ) {
    case ScamData::Keyword:
    case ScamData::String:
    case ScamData::Symbol:
        delete value.strVal;
        break;

    case ScamData::Error:
        delete value.errorData;
        break;

    case ScamData::Pair:
        delete value.pairValue;
        break;

    case ScamData::Vector:
        delete value.vectorData;
        break;

    case ScamData::ByteVector:
        delete value.byteVectorData;
        break;

    case ScamData::Dict:
        delete value.dictData;
        break;

    case ScamData::Closure:
        delete value.closureData;
        break;

    case ScamData::Class:
        delete value.classValue;
        break;

    case ScamData::Instance:
        delete value.instanceData;
        break;

    case ScamData::Primitive:
        delete value.primitiveData;
        break;

    case ScamData::SpecialForm:
        delete value.specialFormData;
        break;

    case ScamData::Port:
        delete value.portData;
        break;

    case ScamData::Syntax:
        delete value.syntaxData;
        break;

    default:
        break;
    }
}

ScamData * ScamData::makeInstance(DataTagType type, bool managed)
{
    return new ScamData(type, managed);
}

void ScamData::mark()
{
    if ( isMarked() ) {
        return;
    }

    ManagedObject::mark();
    if ( metadata ) {
        metadata->mark();
    }

    if ( isNumeric(this) ) {
        if ( isPureComplex(this) ) {
            realPart()->mark();
            imagPart()->mark();
        }
        return;
    }

    switch ( type ) {
    case ScamData::Error:
        for ( auto const & e : errorIrritants() ) {
            e->mark();
        }
        errorCategory()->mark();
        break;

    case ScamData::Pair:
        carValue()->mark();
        cdrValue()->mark();
        break;

    case ScamData::Vector:
        for ( auto const & e : vectorData() ) {
            e->mark();
        }
        break;

    case ScamData::Dict: {
        const DictKeyData & keys   = dictKeys();
        const DictValueData & vals = dictValues();

        size_t size = keys.size();

        for ( size_t idx = 0 ; idx < size ; ++idx ) {
            keys[idx]->mark();
            vals[idx]->mark();
        }
    }
        break;

    case ScamData::Closure:
        closureDef().mark();
        closureEnv()->mark();
        break;

    case ScamData::Class:
        classDef().mark();
        classEnv()->mark();
        break;

    case ScamData::Instance:
        instancePrivate()->mark();
        instanceLocal()->mark();
        break;

    case ScamData::Cont:
        contValue()->mark();
        break;

    case ScamData::Syntax:
        syntaxRules().mark();
        break;

    case ScamData::ScamEnv:
        if ( auto temp = envValue() ) {
            temp->mark();
        }
        break;

    default:
        break;
    }
}

void ScamData::makeImmutable()
{
    immutable = true;
}

bool ScamData::isImmutable() const
{
    return immutable;
}

ScamValue ScamData::setMeta(string const & key, ScamValue value) const
{
    if ( ! metadata ) {
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        metadata = mm.make<Env>();
    }

    ScamValue k = makeSymbol(key);
    ScamValue test = metadata->check(k);
    if ( isUnhandledError(test) ) {
        return test;
    }
    else if ( truth(test) ) {
        (void) metadata->assign(k, value);
    }
    else {
        (void) metadata->put(k, value);
    }

    return makeNothing();
}

ScamValue ScamData::hasMeta(string const & key) const
{
    if ( ! metadata ) {
        return makeBoolean(false);
    }

    ScamValue k = makeSymbol(key);
    return metadata->check(k);
}

ScamValue ScamData::getMeta(string const & key) const
{
    ScamValue rv = makeNull();
    if ( ! metadata ) {
        return rv;
    }

    ScamValue k  = makeSymbol(key);
    ScamValue test = metadata->check(k);
    if ( isError(test) ) {
        rv = test;
    }
    else if ( truth(test) ) {
        rv = metadata->get(k);
    }

    return rv;
}

void ScamData::assertType(DataTagType requiredType)
{
    if ( type != requiredType ) {
        stringstream s;
        s << "InternalError, type assertion in ScamData: expected: "
          << describe(requiredType)
          << "; operating on "
          << describe(type);

        throw ScamException(s.str());
    }
}

bool & ScamData::exactFlag()
{
    if ( 0 == (type & ScamData::Numeric) ) {
        assertType(ScamData::Numeric);
    }
    return value.numericValue->exact;
}

ScamValue & ScamData::realPart()
{
    assertType(ScamData::Complex);
    return value.numericValue->value.complexValue->real;
}

ScamValue & ScamData::imagPart()
{
    assertType(ScamData::Complex);
    return value.numericValue->value.complexValue->imag;
}

double & ScamData::realValue()
{
    assertType(ScamData::Real);
    return value.numericValue->value.realValue;
}

int & ScamData::numPart()
{
    assertType(ScamData::Rational);
    return value.numericValue->value.rationalValue->num;
}

int & ScamData::denPart()
{
    assertType(ScamData::Rational);
    return value.numericValue->value.rationalValue->den;
}

int & ScamData::intPart()
{
    assertType(ScamData::Integer);
    return value.numericValue->value.intValue;
}

bool & ScamData::boolValue()
{
    assertType(ScamData::Boolean);
    return value.boolValue;
}

char & ScamData::charValue()
{
    assertType(ScamData::Character);
    return value.charValue;
}

ScamData::StringData & ScamData::stringValue()
{
    if ( 0 == (type & ScamData::StringLike) ) {
        assertType(ScamData::StringLike);
    }
    return *(value.strVal);
}

ScamData::StringData & ScamData::errorMessage()
{
    assertType(ScamData::Error);
    return value.errorData->msg;
}

ScamData::VectorData & ScamData::errorIrritants()
{
    assertType(ScamData::Error);
    return value.errorData->irritants;
}

bool & ScamData:: errorHandled()
{
    assertType(ScamData::Error);
    return value.errorData->handled;
}

ScamValue & ScamData::errorCategory()
{
    assertType(ScamData::Error);
    return value.errorData->category;
}

ScamValue & ScamData::carValue()
{
    assertType(ScamData::Pair);
    return value.pairValue->car;
}

ScamValue & ScamData::cdrValue()
{
    assertType(ScamData::Pair);
    return value.pairValue->cdr;
}

ScamData::VectorData & ScamData::vectorData()
{
    assertType(ScamData::Vector);
    return *(value.vectorData);
}

ScamData::ByteVectorData & ScamData::byteVectorData()
{
    assertType(ScamData::ByteVector);
    return *(value.byteVectorData);
}

ScamData::DictKeyData & ScamData::dictKeys()
{
    assertType(ScamData::Dict);
    return value.dictData->keys;
}

ScamData::DictValueData & ScamData::dictValues()
{
    assertType(ScamData::Dict);
    return value.dictData->vals;
}

ScamData::ClosureDefType & ScamData::closureDef()
{
    assertType(ScamData::Closure);
    return value.closureData->lambda;
}

Env *& ScamData::closureEnv()
{
    assertType(ScamData::Closure);
    return value.closureData->env;
}

ClassDef & ScamData::classDef()
{
    assertType(ScamData::Class);
    return value.classValue->def;
}

Env *& ScamData::classEnv()
{
    assertType(ScamData::Class);
    return value.classValue->capture;
}

Env *& ScamData::instancePrivate()
{
    assertType(ScamData::Instance);
    return value.instanceData->priv;
}

Env *& ScamData::instanceLocal()
{
    assertType(ScamData::Instance);
    return value.instanceData->local;
}

Continuation *& ScamData::contValue()
{
    assertType(ScamData::Cont);
    return value.contData;
}

std::string & ScamData::primName()
{
    assertType(ScamData::Primitive);
    return value.primitiveData->name;
}

PrimFunction & ScamData::primFunc()
{
    assertType(ScamData::Primitive);
    return value.primitiveData->func;
}

std::string & ScamData::sfName()
{
    assertType(ScamData::SpecialForm);
    return value.specialFormData->name;
}

SfFunction & ScamData::sfFunc()
{
    assertType(ScamData::SpecialForm);
    return value.specialFormData->func;
}

ScamPort *& ScamData::portValue()
{
    assertType(ScamData::Port);
    return value.portData;
}

SyntaxRules & ScamData::syntaxRules()
{
    assertType(ScamData::Syntax);
    return * value.syntaxData;
}

Env *& ScamData::envValue()
{
    assertType(ScamData::ScamEnv);
    return value.envData;
}
