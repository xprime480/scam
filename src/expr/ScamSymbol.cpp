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

void ScamSymbol::eval(ContHandle cont, Env env)
{
    ExprHandle evaluated;

    if ( env.check(this) ) {
        evaluated = env.get(this);
    }
    else {
        stringstream s;
        s << "Symbol " << value << " does not exist in the current environment";
        evaluated = ExpressionFactory::makeError(s.str());
    }

    cont->run(evaluated.get());
}

bool ScamSymbol::isSymbol() const
{
    return true;
}
