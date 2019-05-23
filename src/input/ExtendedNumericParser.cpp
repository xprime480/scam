#include "input/ExtendedNumericParser.hpp"

#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
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

    if ( ! TypePredicates::isNumeric(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}
