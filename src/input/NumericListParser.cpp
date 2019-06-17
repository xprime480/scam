#include "input/NumericListParser.hpp"

#include "input/ListParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

NumericListParser::NumericListParser()
{
    MemoryManager & mm = standardMemoryManager;
    num    = mm.make<ExtendedNumericParser>();
    parser = mm.make<ListParser>(num);
}

NumericListParser * NumericListParser::makeInstance()
{
    return new NumericListParser;
}

void NumericListParser::mark()
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        num->mark();
        parser->mark();
    }
}

bool NumericListParser::accept(ScamValue expr)
{
    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

size_t NumericListParser::size() const
{
    return parser->size();
}

ScamValue NumericListParser::get(size_t idx) const
{
    return parser->get(idx);
}
