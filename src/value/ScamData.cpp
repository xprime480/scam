#include "value/ScamData.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "env/Env.hpp"
#include "form/SyntaxRules.hpp"
#include "port/ScamPort.hpp"
#include "util/ClassDef.hpp"
#include "util/LambdaDef.hpp"
#include "value/ScamNumeric.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"
#include "value/impl/ScamContents.hpp"

using namespace scam;
using namespace scam::ScamData__impl;
using namespace std;

ScamData::ScamData(ScamValueType type, bool managed)
    : ManagedObject(managed)
    , type(type)
    , metadata(nullptr)
    , immutable(true)
    , contents(nullptr)
{
    switch ( type ) {
    case ScamValueType::Boolean:
        contents = new BooleanContents;
        break;

    case ScamValueType::Character:
        contents = new CharacterContents;
        break;

    case ScamValueType::String:
        immutable = false;
        /* fallthrough */

    case ScamValueType::Keyword:
    case ScamValueType::Symbol:
        contents = new StringContents;
        break;

    case ScamValueType::Error:
        contents = new ErrorContents;
        errorCategory() = new ScamData(ScamValueType::Nothing);
        break;

    case ScamValueType::Pair:
        immutable = false;
        contents = new PairContents;
        break;

    case ScamValueType::Vector:
        contents = new VectorContents;
        break;

    case ScamValueType::ByteVector:
        contents = new ByteVectorContents;
        break;

    case ScamValueType::Multiple:
        contents = new MultipleValueContents;
        break;

    case ScamValueType::Cont:
        contents = new ContinuationContents;
        break;

    case ScamValueType::Port:
        contents = new PortContents;
        break;

    case ScamValueType::ScamEnv:
        contents = new EnvContents;
        break;

    case ScamValueType::Syntax:
        contents = new SyntaxContents;
        break;

    case ScamValueType::Dict:
        contents = new DictContents;
        break;

    case ScamValueType::Closure:
        contents = new ClosureContents;
        break;

    case ScamValueType::Class:
        contents = new ClassContents;
        break;

    case ScamValueType::Instance:
        contents = new InstanceContents;
        break;

    case ScamValueType::Primitive:
        contents = new PrimitiveContents;
        break;

    case ScamValueType::SpecialForm:
        contents = new SpecialFormContents;
        break;

    case ScamValueType::Placeholder:
        contents = new PlaceholderContents;
        break;

    case ScamValueType::NaN:
    case ScamValueType::NegInf:
    case ScamValueType::PosInf:
        contents = new NumericContents;
        break;

    case ScamValueType::Complex:
        contents = new ComplexContents;
        break;

    case ScamValueType::Real:
        contents = new RealContents;
        break;

    case ScamValueType::Rational:
        contents = new RationalContents;
        break;

    case ScamValueType::Integer:
        contents = new IntegerContents;
        break;

    default:
        break;
    }
}

ScamData::~ScamData()
{
    delete contents;
    contents = nullptr;
}

ScamData * ScamData::makeInstance(ScamValueType type, bool managed)
{
    return new ScamData(type, managed);
}

void ScamData::mark()
{
    if ( isMarked() ) {
        return;
    }

    ManagedObject::mark();
    safeMark(metadata);
    safeMark(contents);

    if ( isNumeric(this) ) {
        if ( isPureComplex(this) ) {
            safeMark(realPart());
            safeMark(imagPart());
        }
        return;
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

bool & ScamData::boolValue()
{
    BooleanContents * temp = assertContentsType<BooleanContents>();
    return temp->value;
}

unsigned char & ScamData::charValue()
{
    CharacterContents * temp = assertContentsType<CharacterContents>();
    return temp->value;
}

string & ScamData::stringValue()
{
    StringContents * temp = assertContentsType<StringContents>();
    return temp->value;
}

string & ScamData::errorMessage()
{
    ErrorContents * temp = assertContentsType<ErrorContents>();
    return temp->msg;
}

vector<ScamValue> & ScamData::errorIrritants()
{
    ErrorContents * temp = assertContentsType<ErrorContents>();
    return temp->irritants;
}

bool & ScamData:: errorHandled()
{
    ErrorContents * temp = assertContentsType<ErrorContents>();
    return temp->handled;
}

ScamValue & ScamData::errorCategory()
{
    ErrorContents * temp = assertContentsType<ErrorContents>();
    return temp->category;
}

ScamValue & ScamData::carValue()
{
    PairContents * temp = assertContentsType<PairContents>();
    return temp->car;
}

ScamValue & ScamData::cdrValue()
{
    PairContents * temp = assertContentsType<PairContents>();
    return temp->cdr;
}

vector<ScamValue> & ScamData::vectorData()
{
    VectorContents * temp = assertContentsType<VectorContents>();
    return temp->value;
}

vector<unsigned char> & ScamData::byteVectorData()
{
    ByteVectorContents * temp = assertContentsType<ByteVectorContents>();
    return temp->value;
}

vector<ScamValue> & ScamData::multipleValues()
{
    MultipleValueContents * temp = assertContentsType<MultipleValueContents>();
    return temp->value;
}

Continuation *& ScamData::contValue()
{
    ContinuationContents * temp = assertContentsType<ContinuationContents>();
    return temp->value;
}

ScamPort *& ScamData::portValue()
{
    PortContents * temp = assertContentsType<PortContents>();
    return temp->value;
}

Env *& ScamData::envValue()
{
    EnvContents * temp = assertContentsType<EnvContents>();
    return temp->value;
}

SyntaxRules & ScamData::syntaxRules()
{
    SyntaxContents * temp = assertContentsType<SyntaxContents>();
    return temp->value;
}

vector<ScamValue> & ScamData::dictKeys()
{
    DictContents * temp = assertContentsType<DictContents>();
    return temp->keys;
}

vector<ScamValue> & ScamData::dictValues()
{
    DictContents * temp = assertContentsType<DictContents>();
    return temp->vals;
}

LambdaDef & ScamData::closureDef()
{
    ClosureContents * temp = assertContentsType<ClosureContents>();
    return temp->lambda;
}

Env *& ScamData::closureEnv()
{
    ClosureContents * temp = assertContentsType<ClosureContents>();
    return temp->env;
}

ClassDef & ScamData::classDef()
{
    ClassContents * temp = assertContentsType<ClassContents>();
    return temp->def;
}

Env *& ScamData::classEnv()
{
    ClassContents * temp = assertContentsType<ClassContents>();
    return temp->capture;
}

Env *& ScamData::instancePrivate()
{
    InstanceContents * temp = assertContentsType<InstanceContents>();
    return temp->priv;
}

Env *& ScamData::instanceLocal()
{
    InstanceContents * temp = assertContentsType<InstanceContents>();
    return temp->local;
}

std::string & ScamData::primName()
{
    PrimitiveContents * temp = assertContentsType<PrimitiveContents>();
    return temp->fname;
}

PrimFunction & ScamData::primFunc()
{
    PrimitiveContents * temp = assertContentsType<PrimitiveContents>();
    return temp->func;
}

std::string & ScamData::sfName()
{
    SpecialFormContents * temp = assertContentsType<SpecialFormContents>();
    return temp->fname;
}

SfFunction & ScamData::sfFunc()
{
    SpecialFormContents * temp = assertContentsType<SpecialFormContents>();
    return temp->func;
}

ScamValue & ScamData::placeholderValue()
{
    PlaceholderContents * temp = assertContentsType<PlaceholderContents>();
    return temp->value;
}

bool & ScamData::exactFlag()
{
    NumericContents * temp = assertContentsType<NumericContents>();
    if ( ! temp ) {
        stringstream s;
        s << "InternalError, type assertion in ScamData: expected: "
          << "numeric; operating on "
          << describe(type);

        throw ScamException(s.str());
    }
    return temp->exact;
}

ScamValue & ScamData::realPart()
{
    ComplexContents * temp = assertContentsType<ComplexContents>();
    return temp->real;
}

ScamValue & ScamData::imagPart()
{
    ComplexContents * temp = assertContentsType<ComplexContents>();
    return temp->imag;
}

double & ScamData::realValue()
{
    RealContents * temp = assertContentsType<RealContents>();
    return temp->value;
}

int & ScamData::numPart()
{
    RationalContents * temp = assertContentsType<RationalContents>();
    return temp->num;
}

int & ScamData::denPart()
{
    RationalContents * temp = assertContentsType<RationalContents>();
    return temp->den;
}

mpz_t & ScamData::intPart()
{
    IntegerContents * temp = assertContentsType<IntegerContents>();
    return temp->value;
}
