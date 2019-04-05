#include "expr/ScamSymbol.hpp"

#include "Continuation.hpp"
#include "Env.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamSymbol::ScamSymbol(string const & value)
    : value(value)
{
}

ScamSymbol * ScamSymbol::makeInstance(std::string const & value)
{
    return new ScamSymbol(value);
}

string ScamSymbol::toString() const
{
    return value;
}

void ScamSymbol::eval(ContHandle cont, Env env)
{
    ScamExpr * evaluated;

    if ( env.check(this) ) {
        evaluated = env.get(this);
    }
    else {
        stringstream s;
        s << "Symbol " << value << " does not exist in the current environment";
        evaluated = ExpressionFactory::makeError(s.str());
    }

    cont->run(evaluated);
}

bool ScamSymbol::isSymbol() const
{
    return true;
}

bool ScamSymbol::equals(ScamExpr const * expr) const
{
    if ( ! expr->isSymbol() ) {
        return false;
    }
    ScamSymbol const * that = dynamic_cast<ScamSymbol const *>(expr);
    return value == that->value;
}


