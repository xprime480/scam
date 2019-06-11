#include "input/StringTokenizer.hpp"

using namespace scam;
using namespace std;

StringTokenizer::StringTokenizer(string const & input)
    : stream(input)
    , tokenizer(stream)
{
}

void StringTokenizer::mark() const
{
    stream.mark();
    tokenizer.mark();
}

Token StringTokenizer::next()
{
    return tokenizer.next();
}
