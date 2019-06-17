#include "input/CharStreamTokenizer.hpp"

#include "util/NumericConverter.hpp"

#include <algorithm>
#include <sstream>

using namespace scam;
using namespace std;

static const Token none(TokenType::TT_NONE, "");

CharStreamTokenizer::CharStreamTokenizer(CharStream & stream)
    : stream(stream)
    , ok(true)
{
}

CharStreamTokenizer::~CharStreamTokenizer()
{
}

void CharStreamTokenizer::mark()
{
    stream.mark();
}

Token CharStreamTokenizer::next()
{
    static const Token eof(TokenType::TT_END_OF_INPUT, "");

    if ( ! ok ) {
        return eof;
    }

    Token rv;

    rv = scanAtmosphere();
    if ( TokenType::TT_NONE != rv.getType() ) {
        return rv;
    }

    if ( ! stream.peek() ) {
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
    s << "Unable to scan input: {" << stream.allInput(stream.getPos()) << "}";
    Token err(TokenType::TT_SCAN_ERROR, s.str());
    ok = false;
    return err;
}

Token CharStreamTokenizer::scanAtmosphere()
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

bool CharStreamTokenizer::skipWhitespace()
{
    PositionType original = stream.getPos();

    while ( isspace(stream.peek()) ) {
        stream.advance();
    }

    PositionType current = stream.getPos();
    return current != original;
}

bool CharStreamTokenizer::skipSimpleComments()
{
    PositionType original = stream.getPos();

    if ( ';' != stream.peek() ) {
        return false;
    }

    while ( char c = stream.peek() ) {
        stream.advance();
        if ( c == '\n' || !c ) {
            break;
        }
    }

    PositionType current = stream.getPos();
    return current != original;
}

Token CharStreamTokenizer::skipNestedComments()
{
    PositionType original = stream.getPos();
    static const string commentStart { "#|" };
    static const string commentEnd   { "|#" };

    if ( stream.strPeek(2) != commentStart ) {
        return none;
    }
    stream.advance(2);

    while ( stream.peek() ) {
        if ( stream.strPeek(2) == commentEnd ) {
            stream.advance(2);
            Token ok(TokenType::TT_BOOLEAN, "#t");
            return ok;
        }
        else if ( stream.strPeek(2) == commentStart ) {
            Token rv = skipNestedComments();
            if ( TokenType::TT_SCAN_ERROR == rv.getType() ) {
                return rv;
            }
        }
        else {
            stream.advance();
        }
    }

    stringstream s;
    s << "End of input in nested comment: {"
      << stream.allInput(original)
      << "}";
    Token err(TokenType::TT_SCAN_ERROR, s.str());
    return err;
}

bool CharStreamTokenizer::isIdentifierCharacter(char c) const
{
    static const string extended("!$%&*+-./:<=>?@^_~");
    return ( isalnum(c) || string::npos != extended.find(c) );
}

Token CharStreamTokenizer::scanSpecial()
{
    static const string openVector { "#(" };
    static const string openByteLC { "#u8(" };
    static const string openByteUC { "#U8(" };
    static const string splice { ",@" };

    const string peek2 = stream.strPeek(2);
    if ( peek2 == openVector ) {
        static const Token token(TokenType::TT_OPEN_VECTOR, openVector);
        stream.advance(2);
        return token;
    }

    if ( peek2 == splice ) {
        static const Token token(TokenType::TT_SPLICE, splice);
        stream.advance(2);
        return token;
    }

    const string peek4 = stream.strPeek(4);
    if ( peek4 == openByteLC || peek4 == openByteUC ) {
        static const Token token(TokenType::TT_OPEN_BYTE_VECTOR, openByteLC);
        stream.advance(4);
        return token;
    }

    TokenType rv = TokenType::TT_NONE;

    switch ( stream.peek() ) {
    case '(' : rv = TokenType::TT_OPEN_PAREN; break;
    case ')' : rv = TokenType::TT_CLOSE_PAREN; break;
    case '[' : rv = TokenType::TT_OPEN_BRACKET; break;
    case ']' : rv = TokenType::TT_CLOSE_BRACKET; break;
    case '{' : rv = TokenType::TT_OPEN_CURLY; break;
    case '}' : rv = TokenType::TT_CLOSE_CURLY; break;
    case '\'' : rv = TokenType::TT_QUOTE; break;
    case '`' : rv = TokenType::TT_QUASIQUOTE; break;
    case '?' : rv = TokenType::TT_QUESTION; break;
    case ',' : rv = TokenType::TT_UNQUOTE; break;

    case '.' : {
        PositionType original = stream.getPos();
        stream.advance();
        char c2 = stream.peek();
        if ( 0 == c2 || isspace(c2) ) {
            rv = TokenType::TT_DOT;
        }
        stream.setPos(original);
    }
        break;

    default:
        break;
    }

    if ( TokenType::TT_NONE == rv ) {
        return none;
    }

    Token token(rv, stream.strPeek(1));
    stream.advance();
    return token;
}

Token CharStreamTokenizer::scanBoolean()
{
    if ( stream.peek() != '#' ) {
        return none;
    }

    PositionType original = stream.getPos();
    stream.advance();

    while ( ! isDelimiter(stream.peek()) ) {
        stream.advance();
    }

    string text = stream.strBetween(original);
    if ( 1u == text.size() ) {
        stream.setPos(original);
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

    stream.setPos(original);
    return none;
}

Token CharStreamTokenizer::scanCharacter()
{
    const string peek2 = stream.strPeek(2);
    if ( peek2 != "#\\" ) {
        return none;
    }

    PositionType original = stream.getPos();
    stream.advance(2);
    if ( stream.peek() ) {
        stream.advance();
    }

    while ( char c = stream.peek() ) {
        if ( isDelimiter(c) ) {
            break;
        }
        stream.advance();
    }

    string text = stream.strBetween(original);
    if ( 3u != text.size() ) {
        stringstream s;
        s << "Malformed character: {" << text << "}";

        Token err(TokenType::TT_SCAN_ERROR, s.str());
        return err;
    }

    Token token(TokenType::TT_CHARACTER, text.substr(2));
    return token;
}

Token CharStreamTokenizer::scanString()
{
    if ( stream.peek() != '"' ) {
        return none;
    }

    PositionType original = stream.getPos();
    stream.advance();
    PositionType start = stream.getPos();

    while ( char c = stream.peek() ) {
        if ( ! c || '"' == c ) {
            break;
        }
        stream.advance();
    }

    if ( ! stream.peek() ) {
        stringstream s;
        s << "End of input in string: {" << stream.allInput(original) << "}";

        Token err(TokenType::TT_SCAN_ERROR, s.str());
        return err;
    }

    string text = stream.strBetween(start);
    Token token(TokenType::TT_STRING, text);
    stream.advance();
    return token;
}

Token CharStreamTokenizer::scanNumeric()
{
    PositionType original = stream.getPos();

    NumericConverter nc(stream);
    ScamValue expr = nc.getValue();
    if( ! isNumeric(expr) ) {
        return none;
    }

    PositionType pos = stream.getPos();
    if ( ! isDelimiter(stream.peek()) ) {
        stream.setPos(original);
        return none;
    }

    string text = stream.strBetween(original, pos);
    Token token(TokenType::TT_NUMERIC, text, expr);
    return token;
}

Token CharStreamTokenizer::scanKeyword()
{
    PositionType original = stream.getPos();

    if ( ':' != stream.peek() ) {
        return none;
    }
    stream.advance();

    while ( ! isDelimiter(stream.peek()) ) {
        stream.advance();
    }

    string text = stream.strBetween(original);
    Token token(TokenType::TT_KEYWORD, text);
    return token;
}

Token CharStreamTokenizer::scanSymbol()
{
    PositionType original = stream.getPos();

    if ( '|' == stream.peek() ) {
        return scanDelimitedSymbol();
    }

    if ( isdigit(stream.peek()) ) {
        /*
         * The actual spec requires that no prefix be a valid number.
         * This is our best effort for now.
         */
        return none;
    }

    while ( isIdentifierCharacter(stream.peek()) ) {
        stream.advance();
    }

    PositionType current = stream.getPos();
    if ( current == original ) {
        return none;
    }

    string text = stream.strBetween(original);
    Token token(TokenType::TT_SYMBOL, text);
    return token;
}

Token CharStreamTokenizer::scanDelimitedSymbol()
{
    PositionType original = stream.getPos();
    stream.advance();
    PositionType start = stream.getPos();
    PositionType end = start /* updated below */;

    while ( true ) {
        if ( ! stream.peek() ) {
            stringstream s;
            s << "End of input in identifier: {"
              << stream.strBetween(original)
              << "}";
            Token err(TokenType::TT_SCAN_ERROR, s.str());
            return err;
        }

        const char ch = stream.peek();
        if ( '|' == ch ) {
            end = stream.getPos();
            stream.advance();
            break;
        }
        stream.advance();
    }

    string text = stream.strBetween(start, end);
    Token token(TokenType::TT_SYMBOL, text);
    return token;
}
