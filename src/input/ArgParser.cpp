#include "input/ArgParser.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ArgParser::ArgParser()
    : value(ExpressionFactory::makeNull())
{
}

ArgParser * ArgParser::makeInstance()
{
    return new ArgParser;
}

ArgParser::~ArgParser()
{
}

void ArgParser::mark() const
{
    if ( ! isMarked() ) {
        ManagedObject::mark();
        if ( value ) {
            value->mark();
        }
    }
}

bool ArgParser::accept(ScamValue expr)
{
    clearValue();

    if ( nullptr != expr && ! isNull(expr) ) {
        value = expr;
        return true;
    }
    return false;
}

void ArgParser::callback(ScamValue expr)
{
    value = expr;
}

ScamValue ArgParser::getValue() const
{
    return value;
}

void ArgParser::clearValue()
{
    value = ExpressionFactory::makeNull();
}
