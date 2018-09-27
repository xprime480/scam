
#include "input/StringTokenizer.hpp"

#include "input/Token.hpp"

#include <cstring>
#include <sstream>

using namespace scam;
using namespace std;

StringTokenizer::StringTokenizer(std::string const & input)
    : input(input)
    , pos(this->input.c_str())
{
}

Token StringTokenizer::next()
{
    if ( ! pos ) {
        static const Token none(TokenType::TT_NONE, "");
        return none;
    }
    skipWhitespace();
    if ( ! *pos ) {
        static const Token eof(TokenType::TT_END_OF_INPUT, "");
        return eof;
    }

    if ( 0 == strncmp(pos, "#t", 2) ) {
        static const Token tokenTrue(TokenType::TT_BOOLEAN, "#t");
        pos += 2;
        return tokenTrue;
    }
    else if ( 0 == strncmp(pos, "#f", 2) ) {
        static const Token tokenFalse(TokenType::TT_BOOLEAN, "#f");
        pos += 2;
        return tokenFalse;
    }

    static const Token err(TokenType::TT_SCAN_ERROR, pos);
    return err;
}

void StringTokenizer::skipWhitespace()
{
    while ( pos && isspace(*pos) ) {
        ++pos;
    }
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

