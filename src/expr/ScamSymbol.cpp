#include "expr/ScamSymbol.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamSymbol::ScamSymbol(string const & value, bool managed)
    : ScamExpr(ScamData::Symbol, managed)
{
    STRVAL(this) = value;
}

ScamSymbol * ScamSymbol::makeInstance(std::string const & value, bool managed)
{
    return new ScamSymbol(value, managed);
}

void ScamSymbol::eval(Continuation * cont, Env * env) const
{
    ScamValue evaluated;

    if ( env->check(this) ) {
        evaluated = env->get(this);
    }
    else {
        evaluated =
            ExpressionFactory::makeError("Symbol ",
                                         STRVAL(this),
                                         " does not exist",
                                         " in the current environment");
    }

    cont->run(evaluated);
}

bool ScamSymbol::equals(ConstScamValue expr) const
{
    if ( ! isSymbol(expr) ) {
        return false;
    }

    return STRVAL(this) == STRVAL(expr);
}


