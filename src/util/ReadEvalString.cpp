#include "util/ReadEvalString.hpp"

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

ScamValue ReadEvalString::run()
{
    return engine->parseCurrentInput();
}

ScamValue ReadEvalString::read()
{
    return engine->read();
}
