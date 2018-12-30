#if ! defined(TOKEN_H)
#define TOKEN_H 1

#include <string>

namespace scam
{
    enum class TokenType : unsigned char
    {
        TT_NONE = 0,

            TT_OPEN_PAREN,
            TT_CLOSE_PAREN,
            TT_OPEN_BRACKET,
            TT_CLOSE_BRACKET,
            TT_DOT,
            TT_QUOTE,
            TT_QUASIQUOTE,
            TT_UNQUOTE,
            TT_SPLICE,
            TT_QUESTION,

            TT_BOOLEAN,
            TT_CHARACTER,
            TT_STRING,
            TT_SYMBOL,
            TT_KEYWORD,

            TT_FLOAT,
            TT_INTEGER,

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
    OS & operator<<(OS & os, TokenType const & tt)
    {
        switch ( tt ) {
        case TokenType::TT_NONE:
            os << "None";
            break;

        case TokenType::TT_OPEN_PAREN:
            os << "Open Paren";
            break;

        case TokenType::TT_CLOSE_PAREN:
            os << "Close Paren";
            break;

        case TokenType::TT_OPEN_BRACKET:
            os << "Open Bracket";
            break;

        case TokenType::TT_CLOSE_BRACKET:
            os << "Close Bracket";
            break;

        case TokenType::TT_DOT:
            os << "Dot";
            break;

        case TokenType::TT_QUOTE:
            os << "Quote";
            break;

        case TokenType::TT_QUASIQUOTE:
            os << "Quasiquote";
            break;

        case TokenType::TT_UNQUOTE:
            os << "Unquote";
            break;

        case TokenType::TT_SPLICE:
            os << "Splice";
            break;

        case TokenType::TT_BOOLEAN:
            os << "Boolean";
            break;

        case TokenType::TT_CHARACTER:
            os << "Character";
            break;

        case TokenType::TT_STRING:
            os << "String";
            break;

        case TokenType::TT_SYMBOL:
            os << "Symbol";
            break;

        case TokenType::TT_KEYWORD:
            os << "Keyword";
            break;

        case TokenType::TT_FLOAT:
            os << "Float";
            break;

        case TokenType::TT_INTEGER:
            os << "Integer";
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
