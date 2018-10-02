#include "input/ScamParser.hpp"

#include "ScamContext.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamParser::ScamParser(Tokenizer & tokenizer)
    : tokenizer(tokenizer)
{
}

void ScamParser::parseExpr(ScamContext & context) const
{
    shared_ptr<ScamExpr> expr = parseSubExpr();
    context.cont->run(expr);
}

shared_ptr<ScamExpr> ScamParser::parseSubExpr() const
{
    Token t = tokenizer.next();
    return tokenToExpr(t);
}

shared_ptr<ScamExpr> ScamParser::tokenToExpr(Token const & token) const
{
    stringstream s;
    shared_ptr<ScamExpr> rv = ExpressionFactory::makeNull();

    switch ( token.getType() ) {
    case TokenType::TT_NONE:
        rv = ExpressionFactory::makeError("**Internal Error: No Tokens");
        break;
//
//    case TokenType::TT_DOT:
//        throw ScamError("Dot (.) outside list");
//        break;
//
//    case TokenType::TT_TICK:
//        rv = expand_reader_macro("quote");
//        break;
//
//    case TokenType::TT_COMMA:
//        rv = expand_reader_macro("unquote");
//        break;
//
//    case TokenType::TT_OPEN_PAREN:
//        rv = parseList();
//        break;
//
//    case TokenType::TT_CLOSE_PAREN:
//        throw ScamError("Extra ')' in input");
//        break;

    case TokenType::TT_BOOLEAN:
        rv = ExpressionFactory::makeBoolean(token.getText() == "#t");
        break;

    case TokenType::TT_FLOAT:
        rv = ExpressionFactory::makeFloat(std::atof(token.getText().c_str()));
        break;


    case TokenType::TT_INTEGER:
        rv = ExpressionFactory::makeInteger(std::atoi(token.getText().c_str()));
        break;

//
//    case TokenType::TT_STRING:
//        rv = ExpressionFactory::makeString(token.getText());
//        break;
//
//    case TokenType::TT_SYMBOL:
//        rv = ExpressionFactory::makeSymbol(token.getText());
//        break;

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

//Cell ScamParser::parseList() const
//{
//    ScamVector contents;
//
//    while ( true ) {
//        Token token = tokenizer.next();
//
//        TokenType type = token.getType();
//        if ( TokenType::TT_DOT == type )  {
//            Cell last = parseDotContext();
//            if ( last.null() ) {
//                return last;
//            }
//            contents.append(last);
//            contents.set_proper(false);
//            break;
//        }
//        else if ( TokenType::TT_CLOSE_PAREN == type ) {
//            break;
//        }
//        else if ( TokenType::TT_END_OF_INPUT == type ) {
//            throw ScamError("Unterminated List");
//        }
//        else {
//            Cell form = tokenToCell(token);
//            if ( form.null() ) {
//                return form;
//            }
//            contents.append(form);
//        }
//    }
//
//    return ExpressionFactory::makeVector(contents);
//}
//
//Cell ScamParser::parseDotContext() const
//{
//    Token token = tokenizer.next();
//    TokenType type = token.getType();
//
//    if ( TokenType::TT_CLOSE_PAREN == type ) {
//        throw ScamError("No form after '.'");
//    }
//    else if ( TokenType::TT_END_OF_INPUT == type ) {
//        throw ScamError("Unterminated List");
//    }
//
//    Cell next = tokenToCell(token);
//    if ( next.null() ) {
//        return next;
//    }
//
//    Token check = tokenizer.next();
//    TokenType checkType = check.getType();
//    if ( TokenType::TT_END_OF_INPUT == checkType ) {
//        throw ScamError("Unterminated List");
//    }
//    else if ( TokenType::TT_CLOSE_PAREN != checkType ) {
//        throw ScamError("Too many forms after '.'");
//    }
//
//    return next;
//}
//
//Cell ScamParser::expand_reader_macro(std::string const & symbolName) const
//{
//    Cell expr = parseSubExpr();
//    if ( expr.null() ) {
//        std::stringstream s;
//        s << "Error getting form for " << symbolName << " macro";
//        throw ScamError(s.str());
//    }
//
//    ScamVector macro;
//    macro.append(ExpressionFactory::makeSymbol(symbolName));
//    macro.append(expr);
//    return ExpressionFactory::makeVector(macro);
//}
//
//
