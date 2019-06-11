#include "ReplTokenizer.hpp"

#include "input/StringTokenizer.hpp"

using namespace scam;
using namespace std;

ReplTokenizer::ReplTokenizer()
    : pos(0u)
{
}

void ReplTokenizer::mark() const
{
}

Token ReplTokenizer::next()
{
    static Token end(TokenType::TT_END_OF_INPUT, "");
    if ( pos >= buffer.size() ) {
        return end;
    }

    return buffer[pos++];
}

void ReplTokenizer::bufferInput(std::string input)
{
    StringTokenizer tokens(input);

    while ( true ) {
        Token t = tokens.next();
        if ( TokenType::TT_END_OF_INPUT == t.getType() ) {
            break;
        }

        buffer.push_back(t);
    }
}

void ReplTokenizer::flush()
{
    while ( pos > 0 ) {
        buffer.erase(buffer.begin());
        --pos;
    }
}

void ReplTokenizer::restart()
{
    pos = 0;
}


bool ReplTokenizer::empty() const
{
    return buffer.empty();
}
