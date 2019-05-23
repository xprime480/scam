#include "input/UndefineParser.hpp"

#include "expr/ScamSymbol.hpp"
#include "input/SequenceParser.hpp"

using namespace scam;
using namespace std;

UndefineParser::UndefineParser()
{
    MemoryManager & mm = standardMemoryManager;

    sym    = mm.make<SymbolParser>();
    parser = mm.make<SequenceParser>(sym);

    clearValue();
}

UndefineParser * UndefineParser::makeInstance()
{
    return new UndefineParser;
}

void UndefineParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        sym->mark();
        parser->mark();
    }
}

bool UndefineParser::accept(ScamValue expr)
{
    ArgParser::clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

ScamEnvKeyType UndefineParser::getSymbol() const
{
    return dynamic_cast<ScamEnvKeyType>(sym->getValue());
}
