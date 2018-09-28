
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

    Token rv;

    bool keepSkipping { true };
    while ( keepSkipping ) {
        keepSkipping  = skipWhitespace();
        keepSkipping |= skipSimpleComments();

        rv = skipNestedComments();
        if ( TokenType::TT_SCAN_ERROR == rv.getType() ) {
            return rv;
        }
        keepSkipping |= TokenType::TT_BOOLEAN == rv.getType();
    }

    if ( ! *pos ) {
        static const Token eof(TokenType::TT_END_OF_INPUT, "");
        return eof;
    }

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

bool StringTokenizer::skipSimpleComments()
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

Token StringTokenizer::skipNestedComments()
{
    char const * original = pos;
    if ( 0 != strncmp(pos, "#|", 2 ) ) {
        return none;
    }
    pos += 2;

    while ( *pos ) {
        if ( 0 == strncmp(pos, "|#", 2 ) ) {
            pos += 2;
            Token ok(TokenType::TT_BOOLEAN, "#t");
            return ok;
        }
        else if ( 0 == strncmp(pos, "#|", 2) ) {
            Token rv = skipNestedComments();
            if ( TokenType::TT_SCAN_ERROR == rv.getType() ) {
                return rv;
            }
        }
        else {
            ++pos;
        }
    }

    stringstream s;
    s << "End of input in nested comment: {" << original << "}";
    Token err(TokenType::TT_SCAN_ERROR, s.str());
    return err;
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
