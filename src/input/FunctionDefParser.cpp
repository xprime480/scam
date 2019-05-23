#include "input/FunctionDefParser.hpp"

#include "expr/ScamSymbol.hpp"
#include "input/CountedListParser.hpp"
#include "input/LambdaParser.hpp"

using namespace scam;
using namespace std;

FunctionDefParser::FunctionDefParser()
{
    MemoryManager & mm = standardMemoryManager;

    name   = mm.make<SymbolParser>();
    lambda = mm.make<LambdaParser>();
}

FunctionDefParser * FunctionDefParser::makeInstance()
{
    return new FunctionDefParser();
}

void FunctionDefParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();

        name->mark();
        lambda->mark();
    }
}

bool FunctionDefParser::accept(ScamValue expr)
{
    if ( ! ArgParser::accept(expr) ) {
        return false;
    }

    clearValue();

    CountedListParser * temp = getCountedListOfAnythingParser(2, 99999);

    if ( ! temp->accept(expr) ) {
        return false;
    }

    if ( ! name->accept(temp->get(0)) ) {
        return false;
    }

    if ( ! lambda->accept(expr->nthcdr(0)) ) {
        return false;
    }

    callback(expr);
    return true;
}

const ScamSymbol * FunctionDefParser::getName() const
{
    return dynamic_cast<const ScamSymbol *>(name->getValue());
}

const LambdaParser * FunctionDefParser::getLambda() const
{
    return lambda;
}
