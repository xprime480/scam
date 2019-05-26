#include "expr/ScamSymbol.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamSymbol::ScamSymbol(string const & value, bool managed)
    : ScamData(ScamData::Symbol, managed)
{
    STRVAL(this) = value;
}

ScamSymbol * ScamSymbol::makeInstance(std::string const & value, bool managed)
{
    return new ScamSymbol(value, managed);
}
