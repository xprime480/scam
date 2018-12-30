#include "expr/ScamKeyword.hpp"

#include "Continuation.hpp"
#include "Env.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamKeyword::ScamKeyword(string const & value)
    : value(value)
{
}

string ScamKeyword::toString() const
{
    return value;
}

bool ScamKeyword::isKeyword() const
{
    return true;
}
