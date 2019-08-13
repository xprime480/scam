#include "input/Tokenizer.hpp"

#include <cstring>

using namespace scam;
using namespace std;

bool scam::isDelimiter(char c)
{
    static const string delimiters("\"()[]{};#");
    return ( c == '\0' || isspace(c) || string::npos != delimiters.find(c) );
}
