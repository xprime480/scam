#include "input/BindFormParser.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamSymbol.hpp"
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

void BindFormParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        sym->mark();
        parser->mark();
    }
}

bool BindFormParser::accept(ExprHandle expr)
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

ScamEnvKeyType BindFormParser::getSymbol() const
{
    if ( parser->size() ) {
        return dynamic_cast<ScamEnvKeyType>(sym->getValue());
    }
    return nullptr;
}

ExprHandle BindFormParser::getForm() const
{
    if ( parser->size() ) {
        return parser->get(1u);
    }
    return nullptr;
}
