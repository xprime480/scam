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
    ScamValue rv = makeNull();

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
        break;

    case TokenType::TT_END_OF_INPUT:
        rv = makeNull();
        break;

    case TokenType::TT_SCAN_ERROR:
        rv = makeError(token.getText());
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
        return makeError(token.getText());
    }

    if ( TokenType::TT_CLOSE_PAREN == type ) {
        return makeNil();
    }

    if ( TokenType::TT_DOT == type ) {
        return parseDotContext();
    }

    ScamValue car = tokenToExpr(token);
    if ( error(car) ) {
        return car;
    }
    ScamValue cdr = parseList();
    if ( error(cdr) ) {
        return cdr;
    }
    return makeCons(car, cdr);
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
        return makeError(token.getText());
    }

    if ( TokenType::TT_CLOSE_PAREN == type ) {
        return makeError("No form after '.'");
    }

    ScamValue final = tokenToExpr(token);

    Token check = tokenizer.next();
    TokenType checkType = check.getType();

    if ( TokenType::TT_END_OF_INPUT == checkType ) {
        ScamValue err = makeError("Unterminated List");
        tagPartial(err);
        return err;
    }

    if ( TokenType::TT_SCAN_ERROR == checkType ) {
        return makeError(token.getText());
    }

    if ( TokenType::TT_CLOSE_PAREN != checkType ) {
        return makeError("Too many forms after '.'");
    }

    return final;
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
            return makeError(token.getText());
        }

        if ( TokenType::TT_CLOSE_PAREN == type ) {
            return makeVector(vec);
        }

        ScamValue expr = tokenToExpr(token);
        if ( error(expr) ) {
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
            return makeError(token.getText());
        }

        if ( TokenType::TT_CLOSE_PAREN == type ) {
            return makeByteVector(vec);
        }

        ScamValue expr = tokenToExpr(token);
        if ( error(expr) ) {
            return expr;
        }

        if ( ! isInteger(expr) ) {
            ScamValue err = makeError("Non-integer in Byte Vector");
            return err;
        }

        int i = asInteger(expr);
        if ( i < 0 || i > 255 || ! isExact(expr) ) {
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
            return makeError(token.getText());
        }

        if ( TokenType::TT_CLOSE_CURLY == type ) {
            return makeDict(vec);
        }

        ScamValue expr = tokenToExpr(token);
        if ( error(expr) ) {
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
        return makeErrorExtended("Unknown reader macro: ", text);
    }

    ScamValue expr = parseSubExpr();
    if ( isNull(expr) ) {
        return makeErrorExtended("Unterminated macro: ", name);
    }
    if ( error(expr) ) {
        return makeErrorExtended("Error getting form for ",
                                 name,
                                 " macro",
                                 "\t",
                                 writeValue(expr));
    }

    ScamValue sym    = makeSymbol(name);
    ScamValue listed = makeList(expr);
    return makeCons(sym, listed);
}

namespace
{
    void tagPartial(ScamValue expr)
    {
        static string tag = "partial";
        if ( expr ) {
            expr->setMeta(tag, makeNil());
        }
    }
}
