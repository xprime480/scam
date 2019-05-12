#include "input/StringTokenizer.hpp"

#include "expr/ScamExpr.hpp"
#include "input/Token.hpp"
#include "util/NumericConverter.hpp"

#include <algorithm>
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

StringTokenizer::~StringTokenizer()
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

    rv = scanKeyword();
    if ( TokenType::TT_NONE != rv.getType() ) {
        return rv;
    }

    rv = scanSymbol();
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

bool StringTokenizer::isDelimiter(char c)
{
    static const string delimiters("\"()[];#");
    return ( c == '\0' || isspace(c) || string::npos != delimiters.find(c) );
}

bool StringTokenizer::isIdentifierCharacter(char c) const
{
    static const string extended("!$%&*+-./:<=>?@^_~");
    return ( isalnum(c) || string::npos != extended.find(c) );
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

    if ( '{' == *pos ) {
        static const Token token(TokenType::TT_OPEN_CURLY, "{");
        ++pos;
        return token;
    }

    if ( '}' == *pos ) {
        static const Token token(TokenType::TT_CLOSE_CURLY, "}");
        ++pos;
        return token;
    }

    if ( '#' == *pos ) {
        if ( '(' == pos[1] ) {
            static const Token token(TokenType::TT_OPEN_VECTOR, "#(");
            pos += 2;
            return token;
        }
    }

    if ( '.' == *pos ) {
        if ( pos[1] && ! isspace(pos[1]) ) {
            return none;
        }

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

    if ( '?' == *pos ) {
        static const Token token(TokenType::TT_QUESTION, "?");
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

    char const * original = pos;
    ++pos;

    while ( ! isDelimiter(*pos) ) {
        ++pos;
    }

    string text(original, pos-original);
    if ( 1u == text.size() ) {
        pos = original;
        return none;
    }

    string lower_text = text;
    transform(text.begin(), text.end(), lower_text.begin(),
              [](const char c) -> char { return tolower(c); });

    static const Token tokenTrue(TokenType::TT_BOOLEAN, "#t");
    static const Token tokenFalse(TokenType::TT_BOOLEAN, "#f");

    if ( lower_text == "#t" || lower_text == "#true" ) {
        return tokenTrue;
    }

    if ( lower_text == "#f" || lower_text == "#false" ) {
        return tokenFalse;
    }

    if ( 't' == lower_text[1] || 'f' == lower_text[1]  ) {
        stringstream s;
        s << "Malformed boolean: {" << text << "}";

        Token err(TokenType::TT_SCAN_ERROR, s.str());
        return err;
    }

    pos = original;
    return none;
}

Token StringTokenizer::scanCharacter()
{
    if ( 0 != strncmp(pos, "#\\", 2) ) {
        return none;
    }

    char const * original = pos;
    pos += 2;
    if ( *pos ) {
        ++pos;
    }

    while ( ! isDelimiter(*pos) ) {
        ++pos;
    }

    if ( 3u != (pos-original) ) {
        string text(original, pos-original);
        stringstream s;
        s << "Malformed character: {" << text << "}";

        Token err(TokenType::TT_SCAN_ERROR, s.str());
        return err;
    }

    string text(pos-1, 1);
    Token token(TokenType::TT_CHARACTER, text);
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

    NumericConverter nc(pos);
    ExprHandle expr = nc.getValue();
    if( ! expr->isNumeric() ) {
        return none;
    }

    const char * original = pos;
    pos = nc.getPos();

    if ( ! isDelimiter(*pos) ) {
        pos = original;
        return none;
    }

    string text(original, pos-original);
    Token token(TokenType::TT_NUMERIC, text, expr);
    return token;
}

Token StringTokenizer::scanKeyword()
{
    char const * original = pos;

    if ( ':' != *pos ) {
        return none;
    }
    ++pos;

    while ( ! isDelimiter(*pos) ) {
        ++pos;
    }

    string text(original, pos-original);
    Token token(TokenType::TT_KEYWORD, text);
    return token;
}

Token StringTokenizer::scanSymbol()
{
    char const * original = pos;

    if ( '|' == *pos ) {
        return scanDelimitedSymbol();
    }

    if ( isdigit(*pos) ) {
        /*
         * The actual spec requires that no prefix be a valid number.
         * This is our best effort for now.
         */
        return none;
    }

    while ( isIdentifierCharacter(*pos) ) {
        ++pos;
    }

    if ( pos == original ) {
        return none;
    }

    string text = string(original, pos - original);
    Token token(TokenType::TT_SYMBOL, text);
    return token;
}

Token StringTokenizer::scanDelimitedSymbol()
{
    char const * original = pos;

    ++pos;
    while ( true ) {
        if ( ! *pos ) {
            stringstream s;
            s << "End of input in identifier: {" << original << "}";
            Token err(TokenType::TT_SCAN_ERROR, s.str());
            return err;
        }

        const char ch = *pos;
        ++pos;
        if ( '|' == ch ) {
            break;
        }
    }

    string text = string(original + 1, pos - original - 2);
    Token token(TokenType::TT_SYMBOL, text);
    return token;

}
