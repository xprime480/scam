#include "expr/ScamKeyword.hpp"

using namespace scam;
using namespace std;

ScamKeyword::ScamKeyword(string const & value, bool managed)
    : ScamExpr(ScamData::Keyword, managed)
{
    STRVAL(data) = value;
}

ScamKeyword * ScamKeyword::makeInstance(std::string const & value, bool managed)
{
    return new ScamKeyword(value, managed);
}

string ScamKeyword::toString() const
{
    return STRVAL(data);
}

bool ScamKeyword::equals(ConstExprHandle expr) const
{
    if ( ! expr->isKeyword() ) {
        return false;
    }
    ScamKeyword const * that = dynamic_cast<ScamKeyword const *>(expr);
    return STRVAL(data) == STRVAL(that->data);
}

