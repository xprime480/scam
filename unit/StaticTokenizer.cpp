#include "StaticTokenizer.hpp"

using namespace std;
using namespace scam;
using namespace scam::test_impl;

StaticTokenizer::StaticTokenizer(vector<Token> const & tokens)
    : tokens(tokens)
    , index(0)
{
}

void StaticTokenizer::mark()
{
}

Token StaticTokenizer::next()
{
    if ( index >= tokens.size() ) {
        static const Token eof(TokenType::TT_END_OF_INPUT, "");
        return eof;
    }

    Token const & token = tokens[index];
    ++index;
    return token;
}
