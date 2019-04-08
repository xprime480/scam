#include "expr/ScamKeyword.hpp"

#include "Continuation.hpp"
#include "Env.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamKeyword::ScamKeyword(string const & value, bool managed)
    : ScamExpr(managed)
    , value(value)
{
}

ScamKeyword * ScamKeyword::makeInstance(std::string const & value, bool managed)
{
    return new ScamKeyword(value, managed);
}

string ScamKeyword::toString() const
{
    return value;
}

bool ScamKeyword::isKeyword() const
{
    return true;
}

bool ScamKeyword::equals(ScamExpr const * expr) const
{
    if ( ! expr->isKeyword() ) {
        return false;
    }
    ScamKeyword const * that = dynamic_cast<ScamKeyword const *>(expr);
    return value == that->value;
}

