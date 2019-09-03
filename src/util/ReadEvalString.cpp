#include "util/ReadEvalString.hpp"

using namespace scam;
using namespace std;

ReadEvalString::ReadEvalString(string const & text)
    : ReadEval(tokenizer)
    , tokenizer(text)
{
}
