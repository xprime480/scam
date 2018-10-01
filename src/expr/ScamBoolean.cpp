#include "expr/ScamBoolean.hpp"

#include "ScamContext.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamBoolean::ScamBoolean(bool value)
    : value(value)
{
}

string ScamBoolean::toString() const
{
    if ( value ) {
        return "#t";
    }
    return "#f";
}

void ScamBoolean::eval(ScamContext & context)
{
    auto expr = ExpressionFactory::makeBoolean(value);
    context.cont->run(expr);
}

bool ScamBoolean::truth() const
{
    return value;
}
