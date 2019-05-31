#include "input/StringTokenizer.hpp"

using namespace scam;
using namespace std;

StringTokenizer::StringTokenizer(string const & input)
    : stream(input)
    , tokenizer(stream)
{
}

Token StringTokenizer::next()
{
    return tokenizer.next();
}
