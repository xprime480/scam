
#include "input/StringTokenizer.hpp"

#include "input/Token.hpp"

#include <sstream>

using namespace scam;

StringTokenizer::StringTokenizer(std::string const & input)
    : input(input)
    , pos(this->input.c_str())
{
}

Token StringTokenizer::next()
{
    return Token(TokenType::TT_NONE, "");
}

void StringTokenizer::skipWhitespace()
{
}

TokenType StringTokenizer::scanNumericToken(std::string & contents)
{
    return TokenType::TT_NONE;
}

TokenType StringTokenizer::scanString(std::string & contents)
{
    return TokenType::TT_NONE;
}

TokenType StringTokenizer::scanSymbol(std::string & contents)
{
    return TokenType::TT_NONE;
}

