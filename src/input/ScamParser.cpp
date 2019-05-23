#include "input/ScamParser.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"

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
    ScamValue rv = ExpressionFactory::makeNull();

    switch ( token.getType() ) {
    case TokenType::TT_NONE:
        rv = ExpressionFactory::makeError("**Internal Error: No Tokens");
        break;

    case TokenType::TT_DOT:
        rv = ExpressionFactory::makeError("Dot (.) outside list");
        break;

    case TokenType::TT_OPEN_PAREN:
        rv = parseList();
        break;

    case TokenType::TT_CLOSE_PAREN:
        rv = ExpressionFactory::makeError("Extra ')' in input");
        break;

    case TokenType::TT_OPEN_VECTOR:
        rv = parseVector();
        break;

    case TokenType::TT_OPEN_BYTE_VECTOR:
        rv = parseByteVector();
        break;

    case TokenType::TT_CLOSE_BRACKET:
        rv = ExpressionFactory::makeError("Extra ']' in input");
        break;

    case TokenType::TT_OPEN_CURLY:
        rv = parseDict();
        break;

    case TokenType::TT_CLOSE_CURLY:
        rv = ExpressionFactory::makeError("Extra '}' in input");
        break;

    case TokenType::TT_BOOLEAN:
        rv = ExpressionFactory::makeBoolean(token.getText() == "#t");
        break;

    case TokenType::TT_CHARACTER:
        rv = ExpressionFactory::makeCharacter(token.getText());
        break;

    case TokenType::TT_STRING:
        rv = ExpressionFactory::makeString(token.getText());
        break;

    case TokenType::TT_NUMERIC:
        rv = token.getExpr();
        break;

    case TokenType::TT_SYMBOL:
        rv = ExpressionFactory::makeSymbol(token.getText());
        break;

    case TokenType::TT_KEYWORD:
        rv = ExpressionFactory::makeKeyword(token.getText());
        break;

    case TokenType::TT_QUOTE:
    case TokenType::TT_QUASIQUOTE:
    case TokenType::TT_UNQUOTE:
    case TokenType::TT_SPLICE:
        rv = expand_reader_macro(token.getText());
        break;

    case TokenType::TT_QUESTION:
        rv = ExpressionFactory::makeSymbol("backtrack");
        rv = ExpressionFactory::makeList(rv);
        break;

    case TokenType::TT_END_OF_INPUT:
        rv = ExpressionFactory::makeNull();
        break;

    case TokenType::TT_SCAN_ERROR:
        rv = ExpressionFactory::makeError(token.getText());
        break;

    default:
        rv = ExpressionFactory::makeError("**Internal Error:  Unknown token type");
        break;
    }

    return rv;
}

ScamValue ScamParser::parseList() const
{
    Token token = tokenizer.next();
    TokenType type = token.getType();

    if ( TokenType::TT_END_OF_INPUT == type ) {
        ScamValue err = ExpressionFactory::makeError("Unterminated List");
        tagPartial(err);
        return err;
    }

    if ( TokenType::TT_SCAN_ERROR == type ) {
        return ExpressionFactory::makeError(token.getText());
    }

    if ( TokenType::TT_CLOSE_PAREN == type ) {
        return ExpressionFactory::makeNil();
    }

    if ( TokenType::TT_DOT == type ) {
        return parseDotContext();
    }

    ScamValue car = tokenToExpr(token);
    if ( TypePredicates::error(car) ) {
        return car;
    }
    ScamValue cdr = parseList();
    if ( TypePredicates::error(cdr) ) {
        return cdr;
    }
    return ExpressionFactory::makeCons(car, cdr);
}

ScamValue ScamParser::parseDotContext() const
{
    Token token = tokenizer.next();
    TokenType type = token.getType();

    if ( TokenType::TT_END_OF_INPUT == type ) {
        ScamValue err = ExpressionFactory::makeError("Unterminated List");
        tagPartial(err);
        return err;
    }

    if ( TokenType::TT_SCAN_ERROR == type ) {
        return ExpressionFactory::makeError(token.getText());
    }

    if ( TokenType::TT_CLOSE_PAREN == type ) {
        return ExpressionFactory::makeError("No form after '.'");
    }

    ScamValue final = tokenToExpr(token);

    Token check = tokenizer.next();
    TokenType checkType = check.getType();

    if ( TokenType::TT_END_OF_INPUT == checkType ) {
        ScamValue err = ExpressionFactory::makeError("Unterminated List");
        tagPartial(err);
        return err;
    }

    if ( TokenType::TT_SCAN_ERROR == checkType ) {
        return ExpressionFactory::makeError(token.getText());
    }

    if ( TokenType::TT_CLOSE_PAREN != checkType ) {
        return ExpressionFactory::makeError("Too many forms after '.'");
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
            ScamValue err =
                ExpressionFactory::makeError("Unterminated Vector");
            tagPartial(err);
            return err;
        }

        if ( TokenType::TT_SCAN_ERROR == type ) {
            return ExpressionFactory::makeError(token.getText());
        }

        if ( TokenType::TT_CLOSE_PAREN == type ) {
            return ExpressionFactory::makeVector(vec);
        }

        ScamValue expr = tokenToExpr(token);
        if ( TypePredicates::error(expr) ) {
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
            ScamValue err =
                ExpressionFactory::makeError("Unterminated Byte Vector");
            tagPartial(err);
            return err;
        }

        if ( TokenType::TT_SCAN_ERROR == type ) {
            return ExpressionFactory::makeError(token.getText());
        }

        if ( TokenType::TT_CLOSE_PAREN == type ) {
            return ExpressionFactory::makeByteVector(vec);
        }

        ScamValue expr = tokenToExpr(token);
        if ( TypePredicates::error(expr) ) {
            return expr;
        }

        if ( ! TypePredicates::isInteger(expr) ) {
            ScamValue err =
                ExpressionFactory::makeError("Non-integer in Byte Vector");
            return err;
        }

        int i = expr->asInteger();
        if ( i < 0 || i > 255 || ! TypePredicates::isExact(expr) ) {
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
            ScamValue err =
                ExpressionFactory::makeError("Unterminated Dictionary");
            tagPartial(err);
            return err;
        }

        if ( TokenType::TT_SCAN_ERROR == type ) {
            return ExpressionFactory::makeError(token.getText());
        }

        if ( TokenType::TT_CLOSE_CURLY == type ) {
            return ExpressionFactory::makeDict(vec);
        }

        ScamValue expr = tokenToExpr(token);
        if ( TypePredicates::error(expr) ) {
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
        return ExpressionFactory::makeError("Unknown reader macro: ", text);
    }

    ScamValue expr = parseSubExpr();
    if ( TypePredicates::isNull(expr) ) {
        return ExpressionFactory::makeError("Unterminated macro: ", name);
    }
    if ( TypePredicates::error(expr) ) {
        return ExpressionFactory::makeError("Error getting form for ",
                                            name,
                                            " macro",
                                            "\t",
                                            ExprWriter::write(expr));
    }

    ScamValue sym    = ExpressionFactory::makeSymbol(name);
    ScamValue listed = ExpressionFactory::makeList(expr);
    return ExpressionFactory::makeCons(sym, listed);
}

namespace
{
    void tagPartial(ScamValue expr)
    {
        static string tag = "partial";
        static ScamValue const nil = ExpressionFactory::makeNil();

        if ( expr ) {
            expr->setMeta(tag, nil);
        }
    }
}

