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
    : ScamData(type, managed)
{
}

string ScamExpr::toString() const
{
    return ExprWriter::write(this);
}

void ScamExpr::eval(Continuation * cont, Env * env) const
{
    ScamExpr * argggh = const_cast<ScamExpr *>(this);
    cont->run(argggh);
}

bool ScamExpr::hasApply() const
{
    return 0 != (type & ScamData::Applicable);
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
    return type == ScamData::Null;
}

bool ScamExpr::error() const
{
    return type == ScamData::Error;
}

bool ScamExpr::truth() const
{
    if ( isNull() ) {
        return false;
    }
    if ( ! isBoolean() ) {
        return true;
    }

    return BOOLVAL(this);
}

bool ScamExpr::isBoolean() const
{
    return type == ScamData::Boolean;
}

bool ScamExpr::isChar() const
{
    return type == ScamData::Character;
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
    return type == ScamData::String;
}

bool ScamExpr::isSymbol() const
{
    return type == ScamData::Symbol;
}

bool ScamExpr::isKeyword() const
{
    return type == ScamData::Keyword;
}

bool ScamExpr::isNumeric() const
{
    return ScamNumeric::isNumeric(this);
}

bool ScamExpr::isExact() const
{
    return ScamNumeric::isExact(this);
}

bool ScamExpr::isComplex() const
{
    return ScamNumeric::isComplex(this);
}

bool ScamExpr::isReal() const
{
    return ScamNumeric::isReal(this);
}

bool ScamExpr::isRational() const
{
    return ScamNumeric::isRational(this);
}

bool ScamExpr::isInteger() const
{
    return ScamNumeric::isInteger(this);
}

bool ScamExpr::isNaN() const
{
    return ScamNumeric::isNaN(this);
}

bool ScamExpr::isNegInf() const
{
    return ScamNumeric::isNegInf(this);
}

bool ScamExpr::isPosInf() const
{
    return ScamNumeric::isPosInf(this);
}

double ScamExpr::asDouble() const
{
    return ScamNumeric::asDouble(this);
}

pair<int, int> ScamExpr::asRational() const
{
    return ScamNumeric::asRational(this);
}

int ScamExpr::asInteger() const
{
    return ScamNumeric::asInteger(this);
}

ConstExprHandle ScamExpr::realPart() const
{
    // temporary hack!!!
    ConstExprHandle rv = ScamNumeric::realPart(this);
    if ( rv->isNull() ) {
        rv = this;
    }
    return rv;
}

ConstExprHandle ScamExpr::imagPart() const
{
    return ScamNumeric::imagPart(this);
}

bool ScamExpr::isNil() const
{
    return type == ScamData::Nil;
}

bool ScamExpr::isCons() const
{
    return type == ScamData::Cons;
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
    return type == ScamData::Vector;
}

bool ScamExpr::isByteVector() const
{
    return type == ScamData::ByteVector;
}

bool ScamExpr::isProcedure() const
{
    return 0 != (type & ScamData::Procedure);
}

bool ScamExpr::isClass() const
{
    return type == ScamData::Class;
}

bool ScamExpr::isInstance() const
{
    return type == ScamData::Instance;
}

bool ScamExpr::isDict() const
{
    return type == ScamData::Dict;
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

