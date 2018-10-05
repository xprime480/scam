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

string ScamSymbol::toString() const
{
    return value;
}

void ScamSymbol::eval(std::shared_ptr<Continuation> cont, Env & env)
{
    shared_ptr<ScamExpr> me = clone();
    shared_ptr<ScamExpr> evaluated;

    if ( env.check(me) ) {
        evaluated = env.get(me);
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

shared_ptr<ScamExpr> ScamSymbol::clone()
{
    return ExpressionFactory::makeSymbol(value);
}
