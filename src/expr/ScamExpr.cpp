
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
      << " to args " << args->toString();
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

ExprHandle ScamExpr::clone() const
{
    return ExpressionFactory::clone(this);
}
