#include "input/ArgParser.hpp"

#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

ArgParser::ArgParser()
    : value(nullptr)
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

bool ArgParser::accept(ExprHandle expr)
{
    clearValue();

    if ( nullptr != expr && ! expr->isNull() ) {
        value = expr;
        return true;
    }
    return false;
}

void ArgParser::callback(ExprHandle expr)
{
    value = expr;
}

ExprHandle ArgParser::getValue() const
{
    return value;
}

void ArgParser::clearValue()
{
    value = nullptr;
}
