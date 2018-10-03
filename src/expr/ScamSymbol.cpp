#include "expr/ScamSymbol.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamSymbol::ScamSymbol(string const & value)
    : value(value)
{
}

string ScamSymbol::toString() const
{
    return value;
}

bool ScamSymbol::isSymbol() const
{
    return true;
}

shared_ptr<ScamExpr> ScamSymbol::clone()
{
    return ExpressionFactory::makeSymbol(value);
}
