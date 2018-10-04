
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

// // //            TT_OPEN_BRACKET,
// // //            TT_CLOSE_BRACKET,

    case TokenType::TT_DOT:
        rv = ExpressionFactory::makeError("Dot (.) outside list");
        break;

    case TokenType::TT_OPEN_PAREN:
        rv = parseList();
        break;

    case TokenType::TT_CLOSE_PAREN:
        rv = ExpressionFactory::makeError("Extra ')' in input");
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

    case TokenType::TT_FLOAT:
        rv = ExpressionFactory::makeFloat(std::atof(token.getText().c_str()));
        break;

    case TokenType::TT_INTEGER:
        rv = ExpressionFactory::makeInteger(std::atoi(token.getText().c_str()));
        break;

    case TokenType::TT_SYMBOL:
        rv = ExpressionFactory::makeSymbol(token.getText());
        break;

    case TokenType::TT_QUOTE:
    case TokenType::TT_QUASIQUOTE:
    case TokenType::TT_UNQUOTE:
    case TokenType::TT_SPLICE:
        rv = expand_reader_macro(token.getText());
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

shared_ptr<ScamExpr> ScamParser::parseList() const
{
    Token token = tokenizer.next();
    TokenType type = token.getType();

    if ( TokenType::TT_END_OF_INPUT == type ) {
        return ExpressionFactory::makeError("Unterminated List");
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

    shared_ptr<ScamExpr> car = tokenToExpr(token);
    if ( car->error() ) {
        return car;
    }
    shared_ptr<ScamExpr> cdr = parseList();
    if ( cdr->error() ) {
        return cdr;
    }
    return ExpressionFactory::makeCons(car, cdr);
}

shared_ptr<ScamExpr> ScamParser::parseDotContext() const
{
    Token token = tokenizer.next();
    TokenType type = token.getType();

    if ( TokenType::TT_END_OF_INPUT == type ) {
        return ExpressionFactory::makeError("Unterminated List");
    }

    if ( TokenType::TT_SCAN_ERROR == type ) {
        return ExpressionFactory::makeError(token.getText());
    }

    if ( TokenType::TT_CLOSE_PAREN == type ) {
        return ExpressionFactory::makeError("No form after '.'");
    }

    shared_ptr<ScamExpr> final = tokenToExpr(token);

    Token check = tokenizer.next();
    TokenType checkType = check.getType();

    if ( TokenType::TT_END_OF_INPUT == checkType ) {
        return ExpressionFactory::makeError("Unterminated List");
    }

    if ( TokenType::TT_SCAN_ERROR == checkType ) {
        return ExpressionFactory::makeError(token.getText());
    }

    if ( TokenType::TT_CLOSE_PAREN != checkType ) {
        return ExpressionFactory::makeError("Too many forms after '.'");
    }

    return final;
}

shared_ptr<ScamExpr>
ScamParser::expand_reader_macro(std::string const & text) const
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
        stringstream s;
        s <<  "Unknown reader macro: " << text;
        return ExpressionFactory::makeError(s.str());
    }

    shared_ptr<ScamExpr> expr = parseSubExpr();
    if ( expr->isNull() ) {
        stringstream s;
        s << "Unterminated macro: " << name;
        return ExpressionFactory::makeError(s.str());
    }
    if ( expr->error() ) {
        stringstream s;
        s << "Error getting form for " << name << " macro";
        s << "\t" << expr->toString();
        return ExpressionFactory::makeError(s.str());
    }

    shared_ptr<ScamExpr> sym = ExpressionFactory::makeSymbol(name);
    shared_ptr<ScamExpr> nil = ExpressionFactory::makeNil();
    shared_ptr<ScamExpr> listed = ExpressionFactory::makeCons(expr, nil);
    return ExpressionFactory::makeCons(sym, listed);
}
