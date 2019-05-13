#include "input/ExtendedNumericParser.hpp"

#include "expr/ScamExpr.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ExtendedNumericParser::ExtendedNumericParser()
{
}

ExtendedNumericParser * ExtendedNumericParser::makeInstance()
{
    return new ExtendedNumericParser;
}

bool ExtendedNumericParser::accept(ExprHandle expr)
{
    clearValue();

    if ( ! expr->isNumeric() ) {
        return false;
    }

    callback(expr);
    return true;
}
