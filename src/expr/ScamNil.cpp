#include "expr/ScamNil.hpp"

using namespace scam;
using namespace std;

ScamNil::ScamNil()
    : ScamExpr(ScamData::Nil, false)
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

size_t ScamNil::length() const
{
    return 0u;
}

bool ScamNil::equals(ConstExprHandle expr) const
{
    return ( expr && expr->isNil() );
}
