#include "input/SymbolPlusParser.hpp"

#include "input/SequenceParser.hpp"

using namespace scam;
using namespace std;

SymbolPlusParser::SymbolPlusParser()
{
    MemoryManager & mm = standardMemoryManager;

    sym    = mm.make<SymbolParser>();
    form   = mm.make<ArgParser>();
    parser = mm.make<SequenceParser>(sym, form);

    clearValue();
}

SymbolPlusParser * SymbolPlusParser::makeInstance()
{
    return new SymbolPlusParser;
}

void SymbolPlusParser::mark()
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        sym->mark();
        form->mark();
        parser->mark();
    }
}

bool SymbolPlusParser::accept(ScamValue expr)
{
    ArgParser::clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

ScamValue SymbolPlusParser::getSymbol() const
{
    return sym->getValue();
}

ScamValue SymbolPlusParser::getForm() const
{
    return form->getValue();
}
