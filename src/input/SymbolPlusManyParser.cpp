#include "input/SymbolPlusManyParser.hpp"

#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "input/SequenceParser.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

SymbolPlusManyParser::SymbolPlusManyParser()
{
    MemoryManager & mm = standardMemoryManager;
    sym = mm.make<SymbolParser>();
    clearValue();
}

SymbolPlusManyParser * SymbolPlusManyParser::makeInstance()
{
    return new SymbolPlusManyParser;
}

void SymbolPlusManyParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        sym->mark();
        forms->mark();
    }
}

bool SymbolPlusManyParser::accept(ScamValue expr)
{
    if ( ! ArgParser::accept(expr) ) {
        return false;
    }

    clearValue();

    CountedListParser * top = getCountedListOfAnythingParser(1, 99999);
    if ( ! top->accept(expr) ) {
        return false;
    }

    ScamValue first = top->get(0u);
    if ( ! sym->accept(first) ) {
        return false;
    }

    forms = getCdr(expr);

    callback(expr);
    return true;
}

void SymbolPlusManyParser::clearValue()
{
    ArgParser::clearValue();
    forms = makeNothing();
}


ScamValue SymbolPlusManyParser::getSymbol() const
{
    return sym->getValue();
}

ScamValue SymbolPlusManyParser::getForms() const
{
    return forms;
}
