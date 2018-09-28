
#include "input/StringTokenizer.hpp"

#include "input/Token.hpp"

#include <cstring>
#include <sstream>

using namespace scam;
using namespace std;

static const Token none(TokenType::TT_NONE, "");

StringTokenizer::StringTokenizer(string const & input)
    : input(input)
    , pos(this->input.c_str())
{
}

Token StringTokenizer::next()
{
    if ( ! pos ) {
        return none;
    }

    while ( skipWhitespace() || skipComments() ) {
    }

    if ( ! *pos ) {
        static const Token eof(TokenType::TT_END_OF_INPUT, "");
        return eof;
    }

    Token rv;

    rv = scanBoolean();
    if ( TokenType::TT_NONE != rv.getType() ) {
        return rv;
    }

    rv = scanInteger();
    if ( TokenType::TT_NONE != rv.getType() ) {
        return rv;
    }

    stringstream s;
    s << "Unable to scan input: {" << pos << "}";
    Token err(TokenType::TT_SCAN_ERROR, s.str());
    pos += strlen(pos);
    return err;
}

bool StringTokenizer::skipWhitespace()
{
    char const * original = pos;
    while ( pos && isspace(*pos) ) {
        ++pos;
    }
    return pos != original;
}

bool StringTokenizer::skipComments()
{
    char const * original = pos;
    if ( ';' != *pos ) {
        return false;
    }

    while ( *pos && '\n' != *pos ) {
        ++pos;
    }
    return pos != original;
}

bool StringTokenizer::isDelimiter(char c) const
{
    static const string delimiters("\"()[];#");
    return ( c == '\0' || isspace(c) || string::npos != delimiters.find(c) );
}

Token StringTokenizer::scanBoolean()
{
    if ( *pos != '#' ) {
        return none;
    }

    if ( ( 't' == pos[1] || 'T' == pos[1] ) && isDelimiter(pos[2]) ) {
        static const Token tokenTrue(TokenType::TT_BOOLEAN, "#t");
        pos += 2;
        return tokenTrue;
    }
    else if ( ( 'f' == pos[1] || 'F' == pos[1] ) && isDelimiter(pos[2]) ) {
        static const Token tokenFalse(TokenType::TT_BOOLEAN, "#f");
        pos += 2;
        return tokenFalse;
    }

    return none;
}

Token StringTokenizer::scanInteger()
{
    char const * original = pos;

    if ( *pos == '+' || *pos == '-' ) {
        ++pos;
    }

    if ( ! isdigit(*pos) ) {
        pos = original;
        return none;
    }

    while ( isdigit(*pos) ) {
        ++pos;
    }

    if ( ! isDelimiter(*pos) ) {
        pos = original;
        return none;
    }

    string text(original, pos-original);
    Token token(TokenType::TT_INTEGER, text);
    return token;
}

