
#include "expr/ScamFloat.hpp"

#include "ScamContext.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamFloat::ScamFloat(double value)
    : value(value)
{
}

string ScamFloat::toString() const
{
    stringstream s;
    s << value;
    return s.str();
}

void ScamFloat::eval(ScamContext & context)
{
    context.cont->run(ExpressionFactory::makeFloat(value));
}

bool ScamFloat::isFloat() const
{
    return true;
}

double ScamFloat::toFloat() const
{
    return value;
}
