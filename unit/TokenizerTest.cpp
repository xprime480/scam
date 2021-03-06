#include "input/StringTokenizer.hpp"

#include "gtest/gtest.h"

#include <vector>

using namespace scam;
using namespace std;
using namespace ::testing;

namespace
{
    string dump(vector<Token> const & tokens)
    {
        stringstream s;
        for ( Token const & t : tokens ) {
            s << "\t" << t << "\n";
        }
        return s.str();
    }

    AssertionResult
    compareTokenVector(vector<Token> const & exp, vector<Token> const & act)
    {
        bool ok = ( exp.size() == act.size() );

        for ( size_t idx = 0 ; ok && idx < act.size() ; ++idx ) {
            if ( exp[idx] != act[idx] ) {
                ok = false;
            }
        }

        if ( ok ) {
            return AssertionSuccess();
        }

        return AssertionFailure() << "expected " << exp.size()
                                  << " tokens; got " << act.size()
                                  << "\nexpected tokens:\n" << dump(exp)
                                  << "\nactual tokens:\n" << dump(act);
    }

    void string2tokens(string const & s, vector<Token> const & exp)
    {
        vector<Token> act;

        StringTokenizer tokenizer(s);
        while ( true ) {
            Token t = tokenizer.next();
            if ( TokenType::TT_END_OF_INPUT == t.getType() ) {
                break;
            }
            act.push_back(t);
        }

        EXPECT_TRUE(compareTokenVector(exp, act));
    }

    TEST(TokenizerTest, EmptyInput)
    {
        string const input{ "" };
        vector<Token> exp { };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, Whitespace)
    {
        string const input{ "   \
                " };
        vector<Token> exp { };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, SimpleComments)
    {
        string const input{ "; ignore me!!" };
        vector<Token> exp { };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, NestedCommentsSpanLines)
    {
        string const input{ "\n\
#| ignore me!! \n\
This comment style can span lines!\n\
|#" };
        vector<Token> exp { };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, NestedComments2Deep)
    {
        string const input{ "#| nesting #| works! |don't get fooled||# |#" };
        vector<Token> exp { };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, NestedCommentsUnterminated)
    {
        string const input{ "#| simple unterminated." };
        string const msg{ "End of input in nested comment: {#| simple unterminated.}" };
        vector<Token> exp { Token(TokenType::TT_SCAN_ERROR, msg) };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, NestedCommentsOpen2Close1)
    {
        string const input{ "#| #| two in |# one out." };
        string const msg { "End of input in nested comment: {#| #| two in |# one out.}" };
        vector<Token> exp { Token(TokenType::TT_SCAN_ERROR, msg) };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, NestedCommentsOpen2Close0)
    {
        string const input{ "#| #| double plus ungood." };
        string const msg { "End of input in nested comment: {#| double plus ungood.}" };
        vector<Token> exp { Token(TokenType::TT_SCAN_ERROR, msg) };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, SpecialSymbols)
    {
        string const input{ " ( ) [ ] { } #( #u8( . ' ` , ,@ #0= #0#" };
        vector<Token> exp {
            Token(TokenType::TT_OPEN_PAREN,       "(")  ,
            Token(TokenType::TT_CLOSE_PAREN,      ")")  ,
            Token(TokenType::TT_OPEN_BRACKET,     "[")  ,
            Token(TokenType::TT_CLOSE_BRACKET,    "]")  ,
            Token(TokenType::TT_OPEN_CURLY,       "{")  ,
            Token(TokenType::TT_CLOSE_CURLY,      "}")  ,
            Token(TokenType::TT_OPEN_VECTOR,      "#(") ,
            Token(TokenType::TT_OPEN_BYTE_VECTOR, "#u8("),
            Token(TokenType::TT_DOT,              ".")  ,
            Token(TokenType::TT_QUOTE,            "'")  ,
            Token(TokenType::TT_QUASIQUOTE,       "`")  ,
            Token(TokenType::TT_UNQUOTE,          ",")  ,
            Token(TokenType::TT_SPLICE,           ",@") ,
            Token(TokenType::TT_DATUM_DEF,        "#0="),
            Token(TokenType::TT_DATUM_REF,        "#0#")
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, Boolean)
    {
        string const input{ "#t #f #T #F #true #FaLSE" };
        vector<Token> exp {
            Token(TokenType::TT_BOOLEAN, "#t"),
            Token(TokenType::TT_BOOLEAN, "#f"),
            Token(TokenType::TT_BOOLEAN, "#t"),
            Token(TokenType::TT_BOOLEAN, "#f"),
            Token(TokenType::TT_BOOLEAN, "#t"),
            Token(TokenType::TT_BOOLEAN, "#f")
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, BooleanUnterminated)
    {
        string const input{ "#tjunk" };
        vector<Token> exp {
            Token(TokenType::TT_SCAN_ERROR, "Malformed boolean: {#tjunk}"),
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, Characters)
    {
        string const input{ "#\\a#\\Z    #\\+#\\ #\\\\" };
        vector<Token> exp {
            Token(TokenType::TT_CHARACTER, "a"),
            Token(TokenType::TT_CHARACTER, "Z"),
            Token(TokenType::TT_CHARACTER, "+"),
            Token(TokenType::TT_CHARACTER, " "),
            Token(TokenType::TT_CHARACTER, "\\"),
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, CharacterNamed)
    {
        string const input{ "#\\alarm #\\backspace #\\delete #\\escape "
                            "#\\newline #\\null #\\return #\\space #\\tab" };
        vector<Token> exp {
            Token(TokenType::TT_CHARACTER, "\a"),
            Token(TokenType::TT_CHARACTER, "\b"),
            Token(TokenType::TT_CHARACTER, "\x7f"),
            Token(TokenType::TT_CHARACTER, "\e"),
            Token(TokenType::TT_CHARACTER, "\n"),
            Token(TokenType::TT_CHARACTER, string(1u, 0)),
            Token(TokenType::TT_CHARACTER, "\r"),
            Token(TokenType::TT_CHARACTER, " "),
            Token(TokenType::TT_CHARACTER, "\t")
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, CharacterHexValues)
    {
        string const input{ "#\\x00 #\\x20 #\\x30 #\\x41 #\\x3a #\\x3A" };
        vector<Token> exp {
            Token(TokenType::TT_CHARACTER, string(1u, 0)),
            Token(TokenType::TT_CHARACTER, " "),
            Token(TokenType::TT_CHARACTER, "0"),
            Token(TokenType::TT_CHARACTER, "A"),
            Token(TokenType::TT_CHARACTER, ":"),
            Token(TokenType::TT_CHARACTER, ":")
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, CharacterPrefixWithoutValue)
    {
        string const input{ "#\\" };
        vector<Token> exp {
            Token(TokenType::TT_SCAN_ERROR, "Malformed character: {#\\}"),
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, CharacterUnterminated)
    {
        string const input{ "#\\a23" };
        vector<Token> exp {
            Token(TokenType::TT_SCAN_ERROR, "Malformed character: {#\\a23}"),
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, EmptyString)
    {
        string const input{ "\"\"" };
        vector<Token> exp {
            Token(TokenType::TT_STRING, "")
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, NonEmptyString)
    {
        string const input{ "\"Hello, World\"" };
        vector<Token> exp {
            Token(TokenType::TT_STRING, "Hello, World")
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, StringUnterminated)
    {
        string const input{ "\"" };
        string const msg{ "End of input in string: {\"}" };
        vector<Token> exp { Token(TokenType::TT_SCAN_ERROR, msg) };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, Integers)
    {
        string const input{ "1 +3 -5 12342341234141341341341444134" };
        vector<Token> exp {
            Token(TokenType::TT_NUMERIC, "1"),
            Token(TokenType::TT_NUMERIC, "+3"),
            Token(TokenType::TT_NUMERIC, "-5"),
            Token(TokenType::TT_NUMERIC, "12342341234141341341341444134")
        };

        string2tokens(input, exp);
    }

    TEST(TokenizerTest, Reals)
    {
        string const input{ "0.0001 +3.2 -5.01" };
        vector<Token> exp {
            Token(TokenType::TT_NUMERIC, "0.0001"),
            Token(TokenType::TT_NUMERIC, "+3.2"),
            Token(TokenType::TT_NUMERIC, "-5.01")
        };

        return string2tokens(input, exp);
    }

#if 0
    /* I think the description of scanning numeric literals
       invalidates this test, but I'm leaving the code here pending
       further research.
    */
    TEST(TokenizerTest, FalseReals)
    {
        string const input{ ".2" };
        vector<Token> exp {
            Token(TokenType::TT_SYMBOL, ".2")
        };

        return string2tokens(input, exp);
    }
#endif

    TEST(TokenizerTest, Complex)
    {
        string const input{ "1+2i +i 2-nan.0i 1@1" };
        vector<Token> exp {
            Token(TokenType::TT_NUMERIC, "1+2i"),
            Token(TokenType::TT_NUMERIC, "+i"),
            Token(TokenType::TT_NUMERIC, "2-nan.0i"),
            Token(TokenType::TT_NUMERIC, "1@1")
        };

        return string2tokens(input, exp);
    }



    TEST(TokenizerTest, Symbols)
    {
        string const input{ "Two Symbols || | | |23| |Two Symbols| |abc" };
        vector<Token> exp {
            Token(TokenType::TT_SYMBOL, "Two"),
            Token(TokenType::TT_SYMBOL, "Symbols"),
            Token(TokenType::TT_SYMBOL, ""),
            Token(TokenType::TT_SYMBOL, " "),
            Token(TokenType::TT_SYMBOL, "23"),
            Token(TokenType::TT_SYMBOL, "Two Symbols"),
            Token(TokenType::TT_SCAN_ERROR,
                  "End of input in identifier: {|abc}")
        };

        return string2tokens(input, exp);
    }

    TEST(TokenizerTest, QuestionMark)
    {
        string const input{ "? ?" };
        vector<Token> exp {
            Token(TokenType::TT_QUESTION, "?"),
            Token(TokenType::TT_QUESTION, "?")
        };

        return string2tokens(input, exp);
    }

    TEST(TokenizerTest, Keyword)
    {
        string const input{ ":name" };
        vector<Token> exp {
            Token(TokenType::TT_KEYWORD, ":name")
        };

        return string2tokens(input, exp);
    }

    TEST(TokenizerTest, Ellipsis)
    {
        string const input{ "..." };
        vector<Token> exp {
            Token(TokenType::TT_SYMBOL, "...")
        };

        return string2tokens(input, exp);
    }
}
