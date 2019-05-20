#include "input/ScamParser.hpp"

#include "expr/ExpressionFactory.hpp"

#include <vector>

using namespace scam;
using namespace std;

namespace
{
    extern void tagPartial(ExprHandle expr);
}

ScamParser::ScamParser(Tokenizer & tokenizer)
    : tokenizer(tokenizer)
{
}

ExprHandle ScamParser::parseExpr() const
{
    ExprHandle expr = parseSubExpr();
    return expr;
}

ExprHandle ScamParser::parseSubExpr() const
{
    Token t = tokenizer.next();
    return tokenToExpr(t);
}

ExprHandle ScamParser::tokenToExpr(Token const & token) const
{
    ExprHandle rv = ExpressionFactory::makeNull();

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

ExprHandle ScamParser::parseList() const
{
    Token token = tokenizer.next();
    TokenType type = token.getType();

    if ( TokenType::TT_END_OF_INPUT == type ) {
        ExprHandle err = ExpressionFactory::makeError("Unterminated List");
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

    ExprHandle car = tokenToExpr(token);
    if ( car->error() ) {
        return car;
    }
    ExprHandle cdr = parseList();
    if ( cdr->error() ) {
        return cdr;
    }
    return ExpressionFactory::makeCons(car, cdr);
}

ExprHandle ScamParser::parseDotContext() const
{
    Token token = tokenizer.next();
    TokenType type = token.getType();

    if ( TokenType::TT_END_OF_INPUT == type ) {
        ExprHandle err = ExpressionFactory::makeError("Unterminated List");
        tagPartial(err);
        return err;
    }

    if ( TokenType::TT_SCAN_ERROR == type ) {
        return ExpressionFactory::makeError(token.getText());
    }

    if ( TokenType::TT_CLOSE_PAREN == type ) {
        return ExpressionFactory::makeError("No form after '.'");
    }

    ExprHandle final = tokenToExpr(token);

    Token check = tokenizer.next();
    TokenType checkType = check.getType();

    if ( TokenType::TT_END_OF_INPUT == checkType ) {
        ExprHandle err = ExpressionFactory::makeError("Unterminated List");
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

ExprHandle ScamParser::parseVector() const
{
    ExprVec vec;

    while ( true ) {
        Token token = tokenizer.next();
        TokenType type = token.getType();

        if ( TokenType::TT_END_OF_INPUT == type ) {
            ExprHandle err =
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

        ExprHandle expr = tokenToExpr(token);
        if ( expr->error() ) {
            return expr;
        }

        vec.push_back(expr);
    }
}

ExprHandle ScamParser::parseByteVector() const
{
    ByteVec vec;

    while ( true ) {
        Token token = tokenizer.next();
        TokenType type = token.getType();

        if ( TokenType::TT_END_OF_INPUT == type ) {
            ExprHandle err =
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

        ExprHandle expr = tokenToExpr(token);
        if ( expr->error() ) {
            return expr;
        }

        if ( ! expr->isInteger() ) {
            ExprHandle err =
                ExpressionFactory::makeError("Non-integer in Byte Vector");
            return err;
        }

        int i = expr->asInteger();
        if ( i < 0 || i > 255 || ! expr->isExact() ) {
        }

        vec.push_back((unsigned char)i);
    }
}

ExprHandle ScamParser::parseDict() const
{
    ExprVec vec;

    while ( true ) {
        Token token = tokenizer.next();
        TokenType type = token.getType();

        if ( TokenType::TT_END_OF_INPUT == type ) {
            ExprHandle err =
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

        ExprHandle expr = tokenToExpr(token);
        if ( expr->error() ) {
            return expr;
        }

        vec.push_back(expr);
    }
}

ExprHandle ScamParser::expand_reader_macro(std::string const & text) const
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

    ExprHandle expr = parseSubExpr();
    if ( expr->isNull() ) {
        return ExpressionFactory::makeError("Unterminated macro: ", name);
    }
    if ( expr->error() ) {
        return ExpressionFactory::makeError("Error getting form for ",
                                            name,
                                            " macro",
                                            "\t",
                                            expr->toString());
    }

    ExprHandle sym    = ExpressionFactory::makeSymbol(name);
    ExprHandle listed = ExpressionFactory::makeList(expr);
    return ExpressionFactory::makeCons(sym, listed);
}

namespace
{
    void tagPartial(ExprHandle expr)
    {
        static string tag = "partial";
        static ExprHandle const nil = ExpressionFactory::makeNil();

        if ( expr ) {
            expr->setMeta(tag, nil);
        }
    }
}

