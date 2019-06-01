#include "util/ReadEvalStream.hpp"

using namespace scam;

ReadEvalStream::ReadEvalStream(ScamEngine * engine, CharStream & stream)
    : ReadEval(engine, tokenizer)
    , tokenizer(stream)
{
}









