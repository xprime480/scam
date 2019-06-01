#include "util/ReadEvalString.hpp"

using namespace scam;
using namespace std;

ReadEvalString::ReadEvalString(ScamEngine * engine, string const & text)
    : ReadEval(engine, tokenizer)
    , tokenizer(text)
{
}
