#include "util/ReadEvalString.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ReadEvalString::ReadEvalString(ScamEngine * engine, string const & text)
    : engine(engine)
    , tokenizer(text)
{
    engine->pushInput(tokenizer);
}

ReadEvalString::~ReadEvalString()
{
    engine->popInput();
}

ScamExpr * ReadEvalString::run()
{
    return engine->parseCurrentInput();
}

ScamExpr * ReadEvalString::read()
{
    return engine->read();
}
