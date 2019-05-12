#include "input/Token.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Token::Token()
    : type(TokenType::TT_NONE)
    , text("")
    , expr(ExpressionFactory::makeNull())
{
}

Token::Token(TokenType type, string const & text)
    : type(type)
    , text(text)
    , expr(ExpressionFactory::makeNull())
{
}

Token::Token(TokenType type, std::string const & text, ExprHandle expr)
    : type(type)
    , text(text)
    , expr(expr)
{
}

TokenType Token::getType() const
{
    return type;
}

string const & Token::getText() const
{
    return text;
}

ExprHandle Token::getExpr() const
{
    return expr;
}

bool Token::operator==(Token const & rhs) const
{
    return ( ( getType() == rhs.getType() )
             && ( getText() == rhs.getText() ) );
}

bool Token::operator!=(Token const & rhs) const
{
    return ! (*this == rhs);
}

