#include "expr/ScamExpr.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>
#include <utility>

#include "util/DebugTrace.hpp"

using namespace scam;
using namespace std;

ScamExpr::ScamExpr(unsigned long type, bool managed)
    : ManagedObject(managed)
    , data(type)
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

const ScamData & ScamExpr::getData() const
{
    return data;
}

string ScamExpr::toString() const
{
    return ExprWriter::write(data);
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
    return ScamNumeric::isNumeric(data);
}

bool ScamExpr::isExact() const
{
    return ScamNumeric::isExact(data);
}

bool ScamExpr::isComplex() const
{
    return ScamNumeric::isComplex(data);
}

bool ScamExpr::isReal() const
{
    return ScamNumeric::isReal(data);
}

bool ScamExpr::isRational() const
{
    return ScamNumeric::isRational(data);
}

bool ScamExpr::isInteger() const
{
    return ScamNumeric::isInteger(data);
}

bool ScamExpr::isNaN() const
{
    return ScamNumeric::isNaN(data);
}

bool ScamExpr::isNegInf() const
{
    return ScamNumeric::isNegInf(data);
}

bool ScamExpr::isPosInf() const
{
    return ScamNumeric::isPosInf(data);
}

double ScamExpr::asDouble() const
{
    return ScamNumeric::asDouble(data);
}

pair<int, int> ScamExpr::asRational() const
{
    return ScamNumeric::asRational(data);
}

int ScamExpr::asInteger() const
{
    return ScamNumeric::asInteger(data);
}

ConstExprHandle ScamExpr::realPart() const
{
    // temporary hack!!!
    ConstExprHandle rv = ScamNumeric::realPart(data);
    if ( rv->isNull() ) {
        rv = this;
    }
    return rv;
}

ConstExprHandle ScamExpr::imagPart() const
{
    return ScamNumeric::imagPart(data);
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
