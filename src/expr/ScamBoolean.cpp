#include "expr/ScamBoolean.hpp"

#include "ScamContext.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamBoolean::ScamBoolean(bool value)
    : value(value)
{
}

string ScamBoolean::toString() const
{
    if ( value ) {
        return "#t";
    }
    return "#f";
}

bool ScamBoolean::truth() const
{
    return value;
}

shared_ptr<ScamExpr> ScamBoolean::clone()
{
    return ExpressionFactory::makeBoolean(value);
}
