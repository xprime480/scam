#include "expr/ScamSymbol.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamSymbol::ScamSymbol(string const & value, bool managed)
    : ScamExpr(managed)
    , value(value)
{
}

ScamSymbol * ScamSymbol::makeInstance(std::string const & value, bool managed)
{
    return new ScamSymbol(value, managed);
}

string ScamSymbol::toString() const
{
    return value;
}

void ScamSymbol::eval(Continuation * cont, Env * env)
{
    ScamExpr * evaluated;

    if ( env->check(this) ) {
        evaluated = env->get(this);
    }
    else {
        evaluated =
            ExpressionFactory::makeError("Symbol ",
                                         value,
                                         " does not exist",
                                         " in the current environment");
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


