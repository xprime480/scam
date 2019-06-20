#include "input/ScamParser.hpp"

#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

#include <vector>

using namespace scam;
using namespace std;

namespace
{
    extern void tagPartial(ScamValue expr);
}

const ScamValue scam::readErrorCategory = makeSymbol(":read", false);

ScamParser::ScamParser(Tokenizer & tokenizer)
    : tokenizer(tokenizer)
{
}

void ScamParser::mark()
{
    tokenizer.mark();
}

ScamValue ScamParser::parseExpr() const
{
    ScamValue expr = parseSubExpr();
    if ( isError(expr) ) {
        expr->errorCategory() = readErrorCategory;
    }
    return expr;
}

ScamValue ScamParser::parseSubExpr() const
{
    Token t = tokenizer.next();
    return tokenToExpr(t);
}

ScamValue ScamParser::tokenToExpr(Token const & token) const
{
    ScamValue rv = makeNothing();

    switch ( token.getType() ) {
    case TokenType::TT_NONE:
        rv = makeError("**Internal Error: No Tokens");
        break;

    case TokenType::TT_DOT:
        rv = makeError("Dot (.) outside list");
        break;

    case TokenType::TT_OPEN_PAREN:
        rv = parseList();
        break;

    case TokenType::TT_CLOSE_PAREN:
        rv = makeError("Extra ')' in input");
        break;

    case TokenType::TT_OPEN_VECTOR:
        rv = parseVector();
        break;

    case TokenType::TT_OPEN_BYTE_VECTOR:
        rv = parseByteVector();
        break;

    case TokenType::TT_CLOSE_BRACKET:
        rv = makeError("Extra ']' in input");
        break;

    case TokenType::TT_OPEN_CURLY:
        rv = parseDict();
        break;

    case TokenType::TT_CLOSE_CURLY:
        rv = makeError("Extra '}' in input");
        break;

    case TokenType::TT_BOOLEAN:
        rv = makeBoolean(token.getText() == "#t");
        break;

    case TokenType::TT_CHARACTER:
        rv = makeCharacter(token.getText().at(0));
        break;

    case TokenType::TT_STRING:
        rv = makeString(token.getText());
        rv->makeImmutable();
        break;

    case TokenType::TT_NUMERIC:
        rv = token.getExpr();
        break;

    case TokenType::TT_SYMBOL:
        rv = makeSymbol(token.getText());
        break;

    case TokenType::TT_KEYWORD:
        rv = makeKeyword(token.getText());
        break;

    case TokenType::TT_QUOTE:
    case TokenType::TT_QUASIQUOTE:
    case TokenType::TT_UNQUOTE:
    case TokenType::TT_SPLICE:
        rv = expand_reader_macro(token.getText());
        break;

    case TokenType::TT_QUESTION:
        rv = makeSymbol("backtrack");
        rv = makeList(rv);
        rv->makeImmutable();
        break;

    case TokenType::TT_END_OF_INPUT:
        rv = makeEof();
        break;

    case TokenType::TT_SCAN_ERROR:
        rv = makeError(token.getText().c_str());
        break;

    default:
        rv = makeError("**Internal Error:  Unknown token type");
        break;
    }

    return rv;
}

ScamValue ScamParser::parseList() const
{
    Token token = tokenizer.next();
    TokenType type = token.getType();

    if ( TokenType::TT_END_OF_INPUT == type ) {
        ScamValue err = makeError("Unterminated List");
        tagPartial(err);
        return err;
    }

    if ( TokenType::TT_SCAN_ERROR == type ) {
        return makeError(token.getText().c_str());
    }

    if ( TokenType::TT_CLOSE_PAREN == type ) {
        return makeNull();
    }

    if ( TokenType::TT_DOT == type ) {
        return parseDotContext();
    }

    ScamValue car = tokenToExpr(token);
    if ( isError(car) ) {
        return car;
    }

    ScamValue cdr = parseList();
    if ( isError(cdr) ) {
        return cdr;
    }

    ScamValue rv = makePair(car, cdr);
    rv->makeImmutable();
    return rv;
}

ScamValue ScamParser::parseDotContext() const
{
    Token token = tokenizer.next();
    TokenType type = token.getType();

    if ( TokenType::TT_END_OF_INPUT == type ) {
        ScamValue err = makeError("Unterminated List");
        tagPartial(err);
        return err;
    }

    if ( TokenType::TT_SCAN_ERROR == type ) {
        return makeError(token.getText().c_str());
    }

    if ( TokenType::TT_CLOSE_PAREN == type ) {
        return makeError("No form after '.'");
    }

    ScamValue last = tokenToExpr(token);

    Token check = tokenizer.next();
    TokenType checkType = check.getType();

    if ( TokenType::TT_END_OF_INPUT == checkType ) {
        ScamValue err = makeError("Unterminated List");
        tagPartial(err);
        return err;
    }

    if ( TokenType::TT_SCAN_ERROR == checkType ) {
        return makeError(token.getText().c_str());
    }

    if ( TokenType::TT_CLOSE_PAREN != checkType ) {
        return makeError("Too many forms after '.'");
    }

    return last;
}

ScamValue ScamParser::parseVector() const
{
    ExprVec vec;

    while ( true ) {
        Token token = tokenizer.next();
        TokenType type = token.getType();

        if ( TokenType::TT_END_OF_INPUT == type ) {
            ScamValue err = makeError("Unterminated Vector");
            tagPartial(err);
            return err;
        }

        if ( TokenType::TT_SCAN_ERROR == type ) {
            return makeError(token.getText().c_str());
        }

        if ( TokenType::TT_CLOSE_PAREN == type ) {
            return makeVector(vec);
        }

        ScamValue expr = tokenToExpr(token);
        if ( isError(expr) ) {
            return expr;
        }

        vec.push_back(expr);
    }
}

ScamValue ScamParser::parseByteVector() const
{
    ByteVec vec;

    while ( true ) {
        Token token = tokenizer.next();
        TokenType type = token.getType();

        if ( TokenType::TT_END_OF_INPUT == type ) {
            ScamValue err = makeError("Unterminated Byte Vector");
            tagPartial(err);
            return err;
        }

        if ( TokenType::TT_SCAN_ERROR == type ) {
            return makeError(token.getText().c_str());
        }

        if ( TokenType::TT_CLOSE_PAREN == type ) {
            return makeByteVector(vec);
        }

        ScamValue expr = tokenToExpr(token);
        if ( isError(expr) ) {
            return expr;
        }

        if ( ! isInteger(expr) ) {
            return makeError("Bad value in Byte Vector");
        }

        int i = asInteger(expr);
        if ( i < 0 || i > 255 || ! isExact(expr) ) {
            return makeError("Bad value in Byte Vector");
        }

        vec.push_back((unsigned char)i);
    }
}

ScamValue ScamParser::parseDict() const
{
    ExprVec vec;

    while ( true ) {
        Token token = tokenizer.next();
        TokenType type = token.getType();

        if ( TokenType::TT_END_OF_INPUT == type ) {
            ScamValue err = makeError("Unterminated Dictionary");
            tagPartial(err);
            return err;
        }

        if ( TokenType::TT_SCAN_ERROR == type ) {
            return makeError(token.getText().c_str());
        }

        if ( TokenType::TT_CLOSE_CURLY == type ) {
            return makeDict(vec);
        }

        ScamValue expr = tokenToExpr(token);
        if ( isError(expr) ) {
            return expr;
        }

        vec.push_back(expr);
    }
}

ScamValue ScamParser::expand_reader_macro(std::string const & text) const
{
    string name;
    if ( text == "'" ) {
        name = "quote";
    }
    else if ( text == "`" ) {
        name = "quasiquote";
    }
    else if ( text == "," ) {
        name = "unquote";
    }
    else if ( text == ",@" ) {
        name = "splice";
    }
    else {
        return makeError("Unknown reader macro", makeString(text));
    }

    ScamValue expr = parseSubExpr();
    if ( isNothing(expr) ) {
        return makeError("Unterminated reader macro", makeString(name));
    }
    if ( isError(expr) ) {
        return makeError("Error getting form", makeString(name), expr);
    }

    ScamValue sym    = makeSymbol(name);
    ScamValue listed = makeList(expr);
    listed->makeImmutable();
    ScamValue rv = makePair(sym, listed);
    rv->makeImmutable();
    return rv;
}

namespace
{
    void tagPartial(ScamValue expr)
    {
        static string tag = "partial";
        if ( expr ) {
            expr->setMeta(tag, makeNull());
        }
    }
}
