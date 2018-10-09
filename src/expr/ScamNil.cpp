#include "expr/ScamNil.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

string ScamNil::toString() const
{
    static const string value{ "()" };
    return value;
}

bool ScamNil::isNil() const
{
    return true;
}

bool ScamNil::isList() const
{
    return true;
}

size_t ScamNil::length() const
{
    return 0u;
}

ExprHandle ScamNil::clone() const
{
    return ExpressionFactory::makeNil();
}
