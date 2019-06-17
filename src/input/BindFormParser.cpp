#include "input/BindFormParser.hpp"

#include "expr/ValueFactory.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

BindFormParser::BindFormParser()
{
    MemoryManager & mm = standardMemoryManager;

    sym    = mm.make<SymbolParser>();
    parser = getCountedListOfAnythingParser(2, 2);

    clearValue();
}

BindFormParser * BindFormParser::makeInstance()
{
    return new BindFormParser;
}

void BindFormParser::mark()
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        sym->mark();
        parser->mark();
    }
}

bool BindFormParser::accept(ScamValue expr)
{
    ArgParser::clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    if ( ! sym->accept(parser->get(0)) ) {
        return false;
    }

    callback(expr);
    return true;
}

ScamValue BindFormParser::getSymbol() const
{
    return sym->getValue();
}

ScamValue BindFormParser::getForm() const
{
    if ( parser->size() ) {
        return parser->get(1u);
    }
    return makeNothing();
}
