#include "expr/ScamExpr.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>
#include <utility>

#include "util/DebugTrace.hpp"

using namespace scam;
using namespace std;

ScamExpr::ScamExpr(bool managed)
    : ManagedObject(managed)
    , metadata(nullptr)
{
}

void ScamExpr::mark() const
{
    if ( ! isMarked() ) {
        ManagedObject::mark();
        if ( metadata ) {
            metadata->mark();
        }
    }
}

void ScamExpr::eval(Continuation * cont, Env * env) const
{
    ScamExpr * argggh = const_cast<ScamExpr *>(this);
    cont->run(argggh);
}

bool ScamExpr::hasApply() const
{
    return 0 != (data.type & ScamData::Applicable);
}

void ScamExpr::apply(ExprHandle args, Continuation * cont, Env * env)
{
    ExprHandle err =
        ExpressionFactory::makeError("Not possible to apply <",
                                     toString(),
                                     "> to args ",
                                     args->toString());
    cont->run(err);
}

void ScamExpr::mapEval(Continuation * cont, Env * env) const
{
    ScamExpr * argggh = const_cast<ScamExpr *>(this);
    cont->run(argggh);
}

bool ScamExpr::isNull() const
{
    return data.type == ScamData::Null;
}

bool ScamExpr::error() const
{
    return data.type == ScamData::Error;
}

bool ScamExpr::truth() const
{
    if ( isNull() ) {
        return false;
    }
    if ( ! isBoolean() ) {
        return true;
    }

    return BOOLVAL(data);
}

bool ScamExpr::isBoolean() const
{
    return data.type == ScamData::Boolean;
}

bool ScamExpr::isChar() const
{
    return data.type == ScamData::Character;
}

char ScamExpr::toChar() const
{
    stringstream s;
    s << "Cannot convert <" << this->toString() << "> to character";
    throw ScamException(s.str());

    return '\0';
}

bool ScamExpr::isString() const
{
    return data.type == ScamData::String;
}

bool ScamExpr::isSymbol() const
{
    return data.type == ScamData::Symbol;
}

bool ScamExpr::isKeyword() const
{
    return data.type == ScamData::Keyword;
}

bool ScamExpr::isNumeric() const
{
    return 0 != (data.type & ScamData::Numeric);
}

bool ScamExpr::isExact() const
{
    if ( ! isNumeric() ) {
        stringstream s;
        s << "Exactness has no meaning for <" << this->toString() << ">";
        throw ScamException(s.str());
    }

    return EXACT(data);
}

bool ScamExpr::isComplex() const
{
    return ScamData::ComplexBit == (data.type & ScamData::ComplexBit);
}

bool ScamExpr::isReal() const
{
    return ScamData::RealBit == (data.type & ScamData::RealBit);
}

bool ScamExpr::isRational() const
{
    return ScamData::RationalBit == (data.type & ScamData::RationalBit);
}

bool ScamExpr::isInteger() const
{
    return ScamData::IntegerBit == (data.type & ScamData::IntegerBit);
}

bool ScamExpr::isNaN() const
{
    return ScamData::NaNBit == (data.type & ScamData::NaNBit);
}

bool ScamExpr::isNegInf() const
{
    return ScamData::NegInfBit == (data.type & ScamData::NegInfBit);
}

bool ScamExpr::isPosInf() const
{
    return ScamData::PosInfBit == (data.type & ScamData::PosInfBit);
}

double ScamExpr::asDouble() const
{
    stringstream s;
    s << "Cannot convert <" << this->toString() << "> to double";
    throw ScamException(s.str());

    return 0.0;
}

pair<int, int> ScamExpr::asRational() const
{
    stringstream s;
    s << "Cannot convert <" << this->toString() << "> to rational";
    throw ScamException(s.str());

    return make_pair<int, int>(0,0);
}

int ScamExpr::asInteger() const
{
    stringstream s;
    s << "Cannot convert <" << this->toString() << "> to integer";
    throw ScamException(s.str());

    return 0;
}

ConstExprHandle ScamExpr::realPart() const
{
    stringstream s;
    s << "<" << this->toString() << "> has no real part";
    throw ScamException(s.str());

    return this;
}

ConstExprHandle ScamExpr::imagPart() const
{
    stringstream s;
    s << "<" << this->toString() << "> has no imaginary part";
    throw ScamException(s.str());

    return this;
}

bool ScamExpr::isNil() const
{
    return data.type == ScamData::Nil;
}

bool ScamExpr::isCons() const
{
    return data.type == ScamData::Cons;
}

bool ScamExpr::isList() const
{
    if ( isNil() ) {
        return true;
    }
    if ( isCons() ) {
        return getCdr()->isList();
    }

    return false;
}

ExprHandle ScamExpr::getCar() const
{
    stringstream s;
    s << "Cannot take car of <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ExprHandle ScamExpr::getCdr() const
{
    stringstream s;
    s << "Cannot take cdr of <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

bool ScamExpr::isVector() const
{
    return data.type == ScamData::Vector;
}

bool ScamExpr::isByteVector() const
{
    return data.type == ScamData::ByteVector;
}

bool ScamExpr::isProcedure() const
{
    return 0 != (data.type & ScamData::Procedure);
}

bool ScamExpr::isClass() const
{
    return data.type == ScamData::Class;
}

bool ScamExpr::isInstance() const
{
    return data.type == ScamData::Instance;
}

bool ScamExpr::isDict() const
{
    return data.type == ScamData::Dict;
}

size_t ScamExpr::length() const
{
    stringstream s;
    s << "Cannot take the length of <" << this->toString() << ">";
    throw ScamException(s.str());

    return 0u;
}

ExprHandle ScamExpr::nthcar(size_t n) const
{
    stringstream s;
    s << "Cannot index <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ExprHandle ScamExpr::nthcdr(size_t n) const
{
    stringstream s;
    s << "Cannot index <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ExprHandle ScamExpr::withEnvUpdate(Env * updated) const
{
    stringstream s;
    s << "Cannot update env of <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

void ScamExpr::setSelf(ExprHandle expr) const
{
    stringstream s;
    s << "Cannot set self of <" << this->toString() << ">";
    throw ScamException(s.str());
}

void ScamExpr::setParent(ExprHandle expr) const
{
    stringstream s;
    s << "Cannot set parent of <" << this->toString() << ">";
    throw ScamException(s.str());
}

bool ScamExpr::equals(ConstExprHandle expr) const
{
    return this == expr;
}

void ScamExpr::setMeta(string const & key, ExprHandle value) const
{
    if ( ! metadata ) {
        metadata = standardMemoryManager.make<Env>();
    }

    ScamEnvKeyType k = ExpressionFactory::makeSymbol(key);

    if ( metadata->check(k) ) {
        metadata->assign(k, value);
    }
    else {
        metadata->put(k, value);
    }
}

bool ScamExpr::hasMeta(string const & key) const
{
    if ( ! metadata ) {
        return false;
    }

    ScamEnvKeyType k = ExpressionFactory::makeSymbol(key);
    return metadata->check(k);
}

ExprHandle ScamExpr::getMeta(string const & key) const
{
    ExprHandle rv = ExpressionFactory::makeNil();
    if ( ! metadata ) {
        return rv;
    }

    ScamEnvKeyType k  = ExpressionFactory::makeSymbol(key);
    if ( metadata->check(k) ) {
        rv = metadata->get(k);
    }

    return rv;
}
