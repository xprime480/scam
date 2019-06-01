#include "util/ReadEval.hpp"

#include "ScamEngine.hpp"

using namespace scam;
using namespace std;

ReadEval::ReadEval(ScamEngine * engine, Tokenizer & tokenizer)
    : engine(engine)
    , tokenizer(tokenizer)
{
    engine->pushInput(tokenizer);
}

ReadEval::~ReadEval()
{
    engine->popInput();
}

ScamValue ReadEval::run()
{
    return engine->parseCurrentInput();
}

ScamValue ReadEval::read()
{
    return engine->read();
}
