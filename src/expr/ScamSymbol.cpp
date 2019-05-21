#include "expr/ScamSymbol.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamSymbol::ScamSymbol(string const & value, bool managed)
    : ScamExpr(managed)
{
    data.type = ScamData::Symbol;
    STRVAL(data) = value;
}

ScamSymbol * ScamSymbol::makeInstance(std::string const & value, bool managed)
{
    return new ScamSymbol(value, managed);
}

string ScamSymbol::toString() const
{
    return STRVAL(data);
}

void ScamSymbol::eval(Continuation * cont, Env * env) const
{
    ExprHandle evaluated;

    if ( env->check(this) ) {
        evaluated = env->get(this);
    }
    else {
        evaluated =
            ExpressionFactory::makeError("Symbol ",
                                         STRVAL(data),
                                         " does not exist",
                                         " in the current environment");
    }

    cont->run(evaluated);
}

bool ScamSymbol::equals(ConstExprHandle expr) const
{
    if ( ! expr->isSymbol() ) {
        return false;
    }
    ScamSymbol const * that = dynamic_cast<ScamSymbol const *>(expr);
    return STRVAL(data) == STRVAL(that->data);
}


