
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

void ScamExpr::eval(std::shared_ptr<Continuation> cont, Env & env)
{
    shared_ptr<ScamExpr> dup = clone();
    cont->run(dup);
}

bool ScamExpr::hasApply() const
{
    return false;
}

void ScamExpr::apply(std::shared_ptr<ScamExpr> const & args,
                     std::shared_ptr<Continuation> cont,
                     Env & env)
{
    stringstream s;
    s << "Not possible to apply <" << this->toString()
      << " to args " << args->toString();
    shared_ptr<ScamExpr> err = ExpressionFactory::makeError(s.str());
    cont->run(err);
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

shared_ptr<ScamExpr> ScamExpr::getCar() const
{
    stringstream s;
    s << "Cannot take cons of <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

shared_ptr<ScamExpr> ScamExpr::getCdr() const
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

size_t ScamExpr::length() const
{
    stringstream s;
    s << "Cannot take the length of <" << this->toString() << ">";
    throw ScamException(s.str());

    return 0u;
}

std::shared_ptr<ScamExpr> ScamExpr::nth(size_t n) const
{
    stringstream s;
    s << "Cannot index <" << this->toString() << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}
