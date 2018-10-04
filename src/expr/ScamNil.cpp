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

shared_ptr<ScamExpr> ScamNil::clone()
{
    return ExpressionFactory::makeNil();
}
