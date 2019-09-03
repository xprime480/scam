#include "util/ReadEvalStream.hpp"

using namespace scam;

ReadEvalStream::ReadEvalStream(CharStream & stream)
    : ReadEval(tokenizer)
    , tokenizer(stream)
{
}









