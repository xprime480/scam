#include "input/ApplyParser.hpp"

#include "expr/ValueFactory.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

ApplyParser::ApplyParser()
    : parser(getCountedListOfAnythingParser(2, 2))
{
    clearValue();
}

ApplyParser * ApplyParser::makeInstance()
{
    return new ApplyParser;
}

void ApplyParser::mark()
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        parser->mark();
    }
}

bool ApplyParser::accept(ScamValue expr)
{
    ArgParser::clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

ScamValue ApplyParser::getParsedOp() const
{
    if ( 2u == parser->size() ) {
        return parser->get(0);
    }
    return makeNothing();
}

ScamValue ApplyParser::getArgs() const
{
    if ( 2u == parser->size() ) {
        return parser->get(1);
    }
    return makeNothing();
}
