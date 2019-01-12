
#include "expr/ScamFloat.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamFloat::ScamFloat(double value)
    : value(value)
{
}

string ScamFloat::toString() const
{
    stringstream s;
    s << value;
    return s.str();
}

bool ScamFloat::isFloat() const
{
    return true;
}

double ScamFloat::toFloat() const
{
    return value;
}

bool ScamFloat::equals(ScamExpr const * expr) const
{
    if ( ! expr->isFloat() ) {
        return false;
    }
    ScamFloat const * that = dynamic_cast<ScamFloat const *>(expr);
    return ::abs(value - that->value) < 1e-9;
}

