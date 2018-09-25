
#include "input/Token.hpp"

using namespace scam;

Token::Token(TokenType type, std::string const & text)
    : type(type)
    , text(text)
{
}

TokenType Token::getType() const
{
    return type;
}

std::string const & Token::getText() const
{
    return text;
}
