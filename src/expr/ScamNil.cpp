#include "expr/ScamNil.hpp"

#include "expr/TypePredicates.hpp"

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

size_t ScamNil::length() const
{
    return 0u;
}

bool ScamNil::equals(ConstScamValue expr) const
{
    return ( expr && isNil(expr) );
}
