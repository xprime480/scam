
#include "util/EvalString.hpp"

#include <iostream>

using namespace scam;
using namespace std;

EvalString::EvalString(ScamEngine * engine, string const & text)
    : engine(engine)
    , tokenizer(text)
{
    engine->pushInput(tokenizer);
}

EvalString::~EvalString()
{
    engine->popInput();
}

ScamExpr * EvalString::run()
{
    return engine->parseCurrentInput();
}
