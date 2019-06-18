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
    return expr;
}

ScamValue ScamParser::parseSubExpr() const
{
    Token t = tokenizer.next();
    return tokenToExpr(t);
}

ScamValue ScamParser::tokenToExpr(Token const & token) const
{
    static const ScamValue noTokens =
        makeStaticError("**Internal Error: No Tokens");

    static const ScamValue misplacedDot =
        makeStaticError("Dot (.) outside list");

    static const ScamValue extraParen =
        makeStaticError("Extra ')' in input");

    static const ScamValue extraSquare =
        makeStaticError("Extra ']' in input");

    static const ScamValue extraCurly =
        makeStaticError("Extra '}' in input");

    static const ScamValue unknownTokenType =
        makeStaticError("**Internal Error:  Unknown token type");

    ScamValue rv = makeNothing();

    switch ( token.getType() ) {
    case TokenType::TT_NONE:
        rv = noTokens;
        break;

    case TokenType::TT_DOT:
        rv = misplacedDot;
        break;

    case TokenType::TT_OPEN_PAREN:
        rv = parseList();
        break;

    case TokenType::TT_CLOSE_PAREN:
        rv = extraParen;
        break;

    case TokenType::TT_OPEN_VECTOR:
        rv = parseVector();
        break;

    case TokenType::TT_OPEN_BYTE_VECTOR:
        rv = parseByteVector();
        break;

    case TokenType::TT_CLOSE_BRACKET:
        rv = extraSquare;
        break;

    case TokenType::TT_OPEN_CURLY:
        rv = parseDict();
        break;

    case TokenType::TT_CLOSE_CURLY:
        rv = extraCurly;
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
        rv = makeNothing();
        break;

    case TokenType::TT_SCAN_ERROR:
        rv = makeError(token.getText().c_str());
        break;

    default:
        rv = unknownTokenType;
        break;
    }

    return rv;
}

ScamValue ScamParser::parseList() const
{
    static const ScamValue unterminated = makeStaticError("Unterminated List");

    Token token = tokenizer.next();
    TokenType type = token.getType();

    if ( TokenType::TT_END_OF_INPUT == type ) {
        ScamValue err = unterminated;
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
    static const ScamValue unterminated = makeStaticError("Unterminated List");
    static const ScamValue nakedDot = makeStaticError("No form after '.'");
    static const ScamValue extraForms =
        makeStaticError("Too many forms after '.'");

    Token token = tokenizer.next();
    TokenType type = token.getType();

    if ( TokenType::TT_END_OF_INPUT == type ) {
        ScamValue err = unterminated;
        tagPartial(err);
        return err;
    }

    if ( TokenType::TT_SCAN_ERROR == type ) {
        return makeError(token.getText().c_str());
    }

    if ( TokenType::TT_CLOSE_PAREN == type ) {
        return nakedDot;
    }

    ScamValue final = tokenToExpr(token);

    Token check = tokenizer.next();
    TokenType checkType = check.getType();

    if ( TokenType::TT_END_OF_INPUT == checkType ) {
        tagPartial(unterminated);
        return unterminated;
    }

    if ( TokenType::TT_SCAN_ERROR == checkType ) {
        return makeError(token.getText().c_str());
    }

    if ( TokenType::TT_CLOSE_PAREN != checkType ) {
        return extraForms;
    }

    return final;
}

ScamValue ScamParser::parseVector() const
{
    static const ScamValue unterminated =
        makeStaticError("Unterminated Vector");

    ExprVec vec;

    while ( true ) {
        Token token = tokenizer.next();
        TokenType type = token.getType();

        if ( TokenType::TT_END_OF_INPUT == type ) {
            tagPartial(unterminated);
            return unterminated;
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
    static const ScamValue unterminated =
        makeStaticError("Unterminated Byte Vector");
    static const ScamValue badValue =
        makeStaticError("Bad value in Byte Vector");

    ByteVec vec;

    while ( true ) {
        Token token = tokenizer.next();
        TokenType type = token.getType();

        if ( TokenType::TT_END_OF_INPUT == type ) {
            ScamValue err = unterminated;
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
            return badValue;
        }

        int i = asInteger(expr);
        if ( i < 0 || i > 255 || ! isExact(expr) ) {
            return badValue;
        }

        vec.push_back((unsigned char)i);
    }
}

ScamValue ScamParser::parseDict() const
{
    static const ScamValue unterminated =
        makeStaticError("Unterminated Dictionary");

    ExprVec vec;

    while ( true ) {
        Token token = tokenizer.next();
        TokenType type = token.getType();

        if ( TokenType::TT_END_OF_INPUT == type ) {
            tagPartial(unterminated);
            return unterminated;
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
