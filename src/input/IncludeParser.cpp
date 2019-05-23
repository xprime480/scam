#include "input/IncludeParser.hpp"

#include "expr/ScamString.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

IncludeParser::IncludeParser()
{
    MemoryManager & mm = standardMemoryManager;
    str = mm.make<StringParser>();
    parser = mm.make<CountedListParser>(str, 1, 99999);
}

IncludeParser * IncludeParser::makeInstance()
{
    return new IncludeParser;
}

void IncludeParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        str->mark();
        parser->mark();
    }
}

bool IncludeParser::accept(ScamValue expr)
{
    clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

size_t IncludeParser::size() const
{
    return parser->size();
}

ScamValue IncludeParser::get(size_t idx) const
{
    return parser->get(idx);
}
