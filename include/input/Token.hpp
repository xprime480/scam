#if ! defined(TOKEN_H)
#define TOKEN_H 1

#include <string>

namespace scam
{
    enum class TokenType : unsigned char
    {
        TT_NONE = 0,

            TT_BOOLEAN,
            TT_INTEGER,
            TT_STRING,

//            TT_OPEN_PAREN,
//            TT_CLOSE_PAREN,
//            TT_DOT,
//            TT_TICK,
//            TT_COMMA,
//
//            TT_DOUBLE,
//            TT_SYMBOL,
//
            TT_END_OF_INPUT = 254,
            TT_SCAN_ERROR = 255
    };

    class Token
    {
    public:
        Token();
        Token(TokenType type, std::string const & text);

        Token(Token const & rhs) = default;
        Token & operator=(Token const & rhs) = default;

        TokenType getType() const;
        std::string const & getText() const;

        bool operator==(Token const & rhs) const;
        bool operator!=(Token const & rhs) const;

    private:
        TokenType type;
        std::string text;
    };

    template <typename OS>
    OS & operator<<(OS & os, TokenType tt)
    {
        switch ( tt ) {
        case TokenType::TT_NONE:
            os << "None";
            break;

        case TokenType::TT_BOOLEAN:
            os << "Boolean";
            break;

        case TokenType::TT_INTEGER:
            os << "Integer";
            break;

        case TokenType::TT_STRING:
            os << "String";
            break;

        case TokenType::TT_END_OF_INPUT:
            os << "EOF";
            break;

        case TokenType::TT_SCAN_ERROR:
            os << "ERR";
            break;

        default:
            os << "Unknown";
            break;
        }

        return os;
    }

    template <typename OS>
    OS & operator<<(OS & os, Token const & t)
    {
        os << "{Token: " << (t.getType()) << "; <" << (t.getText()) << ">}";
        return os;
    }
}

#endif
