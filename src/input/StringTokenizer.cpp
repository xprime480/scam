
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

    rv = scanAtmosphere();
    if ( TokenType::TT_NONE != rv.getType() ) {
        return rv;
    }

    if ( ! *pos ) {
        static const Token eof(TokenType::TT_END_OF_INPUT, "");
        return eof;
    }

    rv = scanSpecial();
    if ( TokenType::TT_NONE != rv.getType() ) {
        return rv;
    }

    rv = scanBoolean();
    if ( TokenType::TT_NONE != rv.getType() ) {
        return rv;
    }

    rv = scanCharacter();
    if ( TokenType::TT_NONE != rv.getType() ) {
        return rv;
    }

    rv = scanString();
    if ( TokenType::TT_NONE != rv.getType() ) {
        return rv;
    }

    rv = scanNumeric();
    if ( TokenType::TT_NONE != rv.getType() ) {
        return rv;
    }

    stringstream s;
    s << "Unable to scan input: {" << pos << "}";
    Token err(TokenType::TT_SCAN_ERROR, s.str());
    pos += strlen(pos);
    return err;
}

Token StringTokenizer::scanAtmosphere()
{
    bool keepSkipping { true };
    while ( keepSkipping ) {
        keepSkipping  = skipWhitespace();
        keepSkipping |= skipSimpleComments();

        Token rv = skipNestedComments();
        if ( TokenType::TT_SCAN_ERROR == rv.getType() ) {
            return rv;
        }
        keepSkipping |= TokenType::TT_BOOLEAN == rv.getType();
    }

    return none;
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

Token StringTokenizer::scanSpecial()
{
    if ( '(' == *pos ) {
        static const Token token(TokenType::TT_OPEN_PAREN, "(");
        ++pos;
        return token;
    }

    if ( ')' == *pos ) {
        static const Token token(TokenType::TT_CLOSE_PAREN, ")");
        ++pos;
        return token;
    }

    if ( '[' == *pos ) {
        static const Token token(TokenType::TT_OPEN_BRACKET, "[");
        ++pos;
        return token;
    }

    if ( ']' == *pos ) {
        static const Token token(TokenType::TT_CLOSE_BRACKET, "]");
        ++pos;
        return token;
    }

    if ( '.' == *pos ) {
        static const Token token(TokenType::TT_DOT, ".");
        ++pos;
        return token;
    }

    if ( '\'' == *pos ) {
        static const Token token(TokenType::TT_QUOTE, "'");
        ++pos;
        return token;
    }

    if ( '`' == *pos ) {
        static const Token token(TokenType::TT_QUASIQUOTE, "`");
        ++pos;
        return token;
    }

    if ( ',' == *pos ) {
        ++pos;

        if ( '@' == *pos ) {
            static const Token token(TokenType::TT_SPLICE, ",@");
            ++pos;
            return token;
        }

        static const Token token(TokenType::TT_UNQUOTE, ",");
        return token;
    }

    return none;
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

Token StringTokenizer::scanCharacter()
{
    if ( 0 != strncmp(pos, "#\\", 2) ) {
        return none;
    }

    char const * original = pos;
    pos += 2;

    if ( ! *pos ) {
        stringstream s;
        s << "Malformed character: {" << original << "}";

        Token err(TokenType::TT_SCAN_ERROR, s.str());
        return err;
    }

    if ( ! isDelimiter(pos[1]) ) {
        pos = original;
        return none;
    }

    string text(pos, 1);
    Token token(TokenType::TT_CHARACTER, text);
    ++pos;
    return token;
}

Token StringTokenizer::scanString()
{
    if ( *pos != '"' ) {
        return none;
    }

    char const * original = pos;
    ++pos;
    char const * start = pos;

    while ( *pos && '"' != *pos ) {
        ++pos;
    }

    if ( ! *pos ) {
        stringstream s;
        s << "End of input in string: {" << original << "}";

        Token err(TokenType::TT_SCAN_ERROR, s.str());
        return err;
    }

    string text(start, pos-start);
    Token token(TokenType::TT_STRING, text);
    ++pos;
    return token;
}

Token StringTokenizer::scanNumeric()
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
    Token token(TokenType::TT_NUMERIC, text);
    return token;
}
