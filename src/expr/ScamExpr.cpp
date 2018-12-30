
#include "ScamException.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamExpr::~ScamExpr()
{
}

void ScamExpr::eval(ContHandle cont, Env env)
{
    cont->run(this);
}

bool ScamExpr::hasApply() const
{
    return false;
}

void ScamExpr::apply(ScamExpr * args, ContHandle cont, Env env)
{
    stringstream s;
    s << "Not possible to apply <" << this->toString()
      << "> to args " << args->toString();
    ExprHandle err = ExpressionFactory::makeError(s.str());
    cont->run(err.get());
}

void ScamExpr::mapEval(ContHandle cont, Env env)
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

ExprHandle ScamExpr::withEnvUpdate(Env updated) const
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

void ScamExpr::setMeta(string const & key, ScamExpr * value)
{
    ExprHandle ksym = ExpressionFactory::makeSymbol(key);
    ScamExpr * k = ksym.get();

    if ( metadata.check(k) ) {
        metadata.assign(k, value);
    }
    else {
        metadata.put(k, value);
    }
}

bool ScamExpr::hasMeta(string const & key) const
{
    ExprHandle k = ExpressionFactory::makeSymbol(key);
    return metadata.check(k.get());
}

ExprHandle ScamExpr::getMeta(string const & key) const
{
    ExprHandle ksym = ExpressionFactory::makeSymbol(key);
    ScamExpr * k = ksym.get();

    ExprHandle rv = ExpressionFactory::makeNil();
    if ( metadata.check(k) ) {
        rv = metadata.get(k);
    }

    return rv;
}

ExprHandle ScamExpr::clone() const
{
    return ExpressionFactory::clone(this);
}
