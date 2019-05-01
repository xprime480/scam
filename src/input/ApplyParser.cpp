#include "input/ApplyParser.hpp"

#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

ApplyParser::ApplyParser()
    : any(standardMemoryManager.make<ArgParser>())
    , parser(standardMemoryManager.make<CountedListParser>(any, 2, 2))
{
    clearValue();
}

ApplyParser * ApplyParser::makeInstance()
{
    return new ApplyParser;
}

void ApplyParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        any->mark();
        parser->mark();
    }
}

bool ApplyParser::accept(ExprHandle expr)
{
    ArgParser::clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

ExprHandle ApplyParser::getParsedOp() const
{
    if ( 2u == parser->size() ) {
        return parser->get(0);
    }
    return nullptr;
}

ExprHandle ApplyParser::getArgs() const
{
    if ( 2u == parser->size() ) {
        return parser->get(1);
    }
    return nullptr;
}
