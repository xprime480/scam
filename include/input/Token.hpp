#if ! defined(TOKEN_H)
#define TOKEN_H 1

#include <string>

namespace scam
{
    enum class TokenType : unsigned char
    {
        TT_NONE = 0,

//            TT_OPEN_PAREN,
//            TT_CLOSE_PAREN,
//            TT_DOT,
//            TT_TICK,
//            TT_COMMA,
//
//            TT_STRING,
//            TT_INTEGER,
//            TT_DOUBLE,
//            TT_SYMBOL,
//
//            TT_END_OF_INPUT = 254,
//            TT_SCAN_ERROR = 255
    };

    class Token
    {
    public:
        Token(TokenType type, std::string const & text);

        TokenType getType() const;
        std::string const & getText() const;

    private:
        TokenType type;
        std::string const text;
    };
}

#endif
