#include "expr/ScamNil.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamNil::ScamNil()
    : ScamExpr(false)
{
}

ScamNil * ScamNil::makeInstance()
{
    static ScamNil nil;
    return &nil;
}

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

bool ScamNil::equals(ScamExpr const * expr) const
{
    return ( expr && expr->isNil() );
}
