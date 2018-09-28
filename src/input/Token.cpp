
#include "input/Token.hpp"

using namespace scam;
using namespace std;

Token::Token()
    : type(TokenType::TT_NONE)
    , text("")
{
}

Token::Token(TokenType type, string const & text)
    : type(type)
    , text(text)
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

bool Token::operator==(Token const & rhs) const
{
    return ( ( getType() == rhs.getType() )
             && ( getText() == rhs.getText() ) );
}

bool Token::operator!=(Token const & rhs) const
{
    return ! (*this == rhs);
}

