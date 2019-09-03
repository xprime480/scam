#include "util/ReadEval.hpp"

#include "ScamEngine.hpp"

using namespace scam;
using namespace std;

ReadEval::ReadEval(Tokenizer & tokenizer)
    : tokenizer(tokenizer)
{
    ScamEngine::getEngine().pushInput(tokenizer);
}

ReadEval::~ReadEval()
{
    ScamEngine::getEngine().popInput();
}

ScamValue ReadEval::run()
{
    return ScamEngine::getEngine().readEvalCurrent();
}

ScamValue ReadEval::read()
{
    return ScamEngine::getEngine().read();
}
