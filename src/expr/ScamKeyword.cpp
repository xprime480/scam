#include "expr/ScamKeyword.hpp"

#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ScamKeyword::ScamKeyword(string const & value, bool managed)
    : ScamExpr(ScamData::Keyword, managed)
{
    STRVAL(this) = value;
}

ScamKeyword * ScamKeyword::makeInstance(std::string const & value, bool managed)
{
    return new ScamKeyword(value, managed);
}
