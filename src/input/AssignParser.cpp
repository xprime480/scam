#include "input/AssignParser.hpp"

#include "expr/ScamSymbol.hpp"
#include "input/SequenceParser.hpp"

using namespace scam;
using namespace std;

AssignParser::AssignParser()
{
    MemoryManager & mm = standardMemoryManager;

    sym    = mm.make<SymbolParser>();
    form   = mm.make<ArgParser>();
    parser = mm.make<SequenceParser>(sym, form);

    clearValue();
}

AssignParser * AssignParser::makeInstance()
{
    return new AssignParser;
}

void AssignParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        sym->mark();
        form->mark();
        parser->mark();
    }
}

bool AssignParser::accept(ExprHandle expr)
{
    ArgParser::clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

ScamEnvKeyType AssignParser::getSymbol() const
{
    return dynamic_cast<ScamEnvKeyType>(sym->getValue());
}

ExprHandle AssignParser::getForm() const
{
    return form->getValue();
}
