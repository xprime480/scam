#include "input/SymbolPlusParser.hpp"

#include "expr/ScamSymbol.hpp"
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

void SymbolPlusParser::mark() const
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

ScamEnvKeyType SymbolPlusParser::getSymbol() const
{
    return dynamic_cast<ScamEnvKeyType>(sym->getValue());
}

ScamValue SymbolPlusParser::getForm() const
{
    return form->getValue();
}
