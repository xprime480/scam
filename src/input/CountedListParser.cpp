#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

CountedListParser::CountedListParser(ArgParser * itemParser,
                                     size_t min,
                                     size_t max)
    : ListParser(itemParser)
    , min(min)
    , max(max)
{
}

CountedListParser *
CountedListParser::makeInstance(ArgParser * itemParser, size_t min, size_t max)
{
    return new CountedListParser(itemParser, min, max);
}

bool CountedListParser::accept(ExprHandle expr)
{
    if ( ! ListParser::accept(expr) ) {
        return false;
    }

    const size_t count = size();
    if ( count < min || count > max ) {
        clearValue();
        return false;
    }

    return true;
}
