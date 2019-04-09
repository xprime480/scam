
#include "ScamException.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamExpr::ScamExpr(bool managed)
    : ManagedObject(managed)
{
}

void ScamExpr::eval(Continuation * cont, Env env)
{
    cont->run(this);
}

bool ScamExpr::hasApply() const
{
    return false;
}

void ScamExpr::apply(ScamExpr * args, Continuation * cont, Env env)
{
    stringstream s;
    s << "Not possible to apply <" << this->toString()
      << "> to args " << args->toString();
    ScamExpr * err = ExpressionFactory::makeError(s.str());
    cont->run(err);
}

void ScamExpr::mapEval(Continuation * cont, Env env)
{
    cont->run(this);
}

bool ScamExpr::isNull() const
{
    return false;
}

bool ScamExpr::error() const
{
    return false;
}

bool ScamExpr::truth() const
{
    return true;
}

bool ScamExpr::isBoolean() const
{
    return false;
}

bool ScamExpr::isChar() const
{
    return false;
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
    return false;
}

bool ScamExpr::isSymbol() const
{
    return false;
}

bool ScamExpr::isKeyword() const
{
    return false;
}

bool ScamExpr::isNumeric() const
{
    return false;
}

bool ScamExpr::isFloat() const
{
    return false;
}

double ScamExpr::toFloat() const
{
    stringstream s;
    s << "Cannot convert <" << this->toString() << "> to float";
    throw ScamException(s.str());

    return 0.0;
}

bool ScamExpr::isInteger() const
{
    return false;
}

int ScamExpr::toInteger() const
{
    stringstream s;
    s << "Cannot convert <" << this->toString() << "> to integer";
    throw ScamException(s.str());

    return 0;
}

bool ScamExpr::isNil() const
{
    return false;
}

bool ScamExpr::isCons() const
{
    return false;
}

bool ScamExpr::isList() const
{
    return false;
}

ScamExpr * ScamExpr::getCar() const
{
    stringstream s;
    s << "Cannot take car of <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ScamExpr * ScamExpr::getCdr() const
{
    stringstream s;
    s << "Cannot take cdr of <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

bool ScamExpr::isVector() const
{
    return false;
}

bool ScamExpr::isProcedure() const
{
    return false;
}

bool ScamExpr::isClass() const
{
    return false;
}

bool ScamExpr::isInstance() const
{
    return false;
}

bool ScamExpr::isDict() const
{
    return false;
}

size_t ScamExpr::length() const
{
    stringstream s;
    s << "Cannot take the length of <" << this->toString() << ">";
    throw ScamException(s.str());

    return 0u;
}

ScamExpr * ScamExpr::nthcar(size_t n) const
{
    stringstream s;
    s << "Cannot index <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ScamExpr * ScamExpr::nthcdr(size_t n) const
{
    stringstream s;
    s << "Cannot index <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ScamExpr * ScamExpr::withEnvUpdate(Env updated) const
{
    stringstream s;
    s << "Cannot update env of <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

void ScamExpr::setSelf(ScamExpr * expr) const
{
    stringstream s;
    s << "Cannot set self of <" << this->toString() << ">";
    throw ScamException(s.str());
}

void ScamExpr::setParent(ScamExpr * expr) const
{
    stringstream s;
    s << "Cannot set parent of <" << this->toString() << ">";
    throw ScamException(s.str());
}

bool ScamExpr::equals(ScamExpr const * expr) const
{
    return this == expr;
}


void ScamExpr::setMeta(string const & key, ScamExpr * value)
{
    ScamExpr * k = ExpressionFactory::makeSymbol(key);

    if ( metadata.check(k) ) {
        metadata.assign(k, value);
    }
    else {
        metadata.put(k, value);
    }
}

bool ScamExpr::hasMeta(string const & key) const
{
    ScamExpr * k = ExpressionFactory::makeSymbol(key);
    return metadata.check(k);
}

ScamExpr * ScamExpr::getMeta(string const & key) const
{
    ScamExpr * k = ExpressionFactory::makeSymbol(key);
    ScamExpr * rv = ExpressionFactory::makeNil();
    if ( metadata.check(k) ) {
        rv = metadata.get(k);
    }

    return rv;
}
