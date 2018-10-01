
#include "input/StringTokenizer.hpp"

#include <vector>

using namespace scam;
using namespace std;

namespace
{
    void dump(string const & tag, vector<Token> const & tokens)
    {
        cerr << tag << "\n";
        for ( Token const & t : tokens ) {
            cerr << "\t" << t << "\n";
        }
    }

    bool string2tokens(string const & s, vector<Token> const & exp)
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

        if ( act.size() != exp.size() ) {
            cerr << "Expected " << exp.size()
                 << "; got " << act.size()
                 << " tokens\n";
            dump("actual", act);
            dump("expected", exp);
            return false;
        }

        bool ok { true };
        for ( size_t idx = 0 ; idx < act.size() ; ++idx ) {
            if ( act[idx] != exp[idx] ) {
                cerr << "Token #" << (1+idx)
                     << ": expected " << exp[idx]
                     << " got: " << act[idx] << "\n";
                ok = false;
            }
        }

        return ok;
    }

    bool emptytest()
    {
        string const input{ "" };
        vector<Token> exp { };

        return string2tokens(input, exp);
    }

    bool whitespace()
    {
        string const input{ "   \
                " };
        vector<Token> exp { };

        return string2tokens(input, exp);
    }

    bool simplecomments()
    {
        string const input{ "; ignore me!!" };
        vector<Token> exp { };

        return string2tokens(input, exp);
    }

    bool nestedcomments1()
    {
        string const input{ "\n\
#| ignore me!! \n\
This comment style can span lines!\n\
|#" };
        vector<Token> exp { };

        return string2tokens(input, exp);
    }

    bool nestedcomments2()
    {
        string const input{ "#| nesting #| works! |don't get fooled||# |#" };
        vector<Token> exp { };

        return string2tokens(input, exp);
    }

    bool nestedcommentsbad1()
    {
        string const input{ "#| simple unterminated." };
        string const msg{ "End of input in nested comment: {#| simple unterminated.}" };
        vector<Token> exp { Token(TokenType::TT_SCAN_ERROR, msg) };

        return string2tokens(input, exp);
    }

    bool nestedcommentsbad2()
    {
        string const input{ "#| #| two in |# one out." };
        string const msg { "End of input in nested comment: {#| #| two in |# one out.}" };
        vector<Token> exp { Token(TokenType::TT_SCAN_ERROR, msg) };

        return string2tokens(input, exp);
    }

    bool nestedcommentsbad3()
    {
        string const input{ "#| #| double plus ungood." };
        string const msg { "End of input in nested comment: {#| double plus ungood.}" };
        vector<Token> exp { Token(TokenType::TT_SCAN_ERROR, msg) };

        return string2tokens(input, exp);
    }

    bool specials()
    {
        string const input{ "()[].'`,,@" };
        vector<Token> exp {
            Token(TokenType::TT_OPEN_PAREN,    "(") ,
            Token(TokenType::TT_CLOSE_PAREN,   ")") ,
            Token(TokenType::TT_OPEN_BRACKET,  "[") ,
            Token(TokenType::TT_CLOSE_BRACKET, "]") ,
            Token(TokenType::TT_DOT,           ".") ,
            Token(TokenType::TT_QUOTE,         "'") ,
            Token(TokenType::TT_QUASIQUOTE,    "`") ,
            Token(TokenType::TT_UNQUOTE,       ",") ,
            Token(TokenType::TT_SPLICE,        ",@") ,
       };

        return string2tokens(input, exp);
    }

    bool booleans()
    {
        string const input{ "#t #f #T #F" };
        vector<Token> exp {
            Token(TokenType::TT_BOOLEAN, "#t"),
            Token(TokenType::TT_BOOLEAN, "#f"),
            Token(TokenType::TT_BOOLEAN, "#t"),
            Token(TokenType::TT_BOOLEAN, "#f"),
        };

        return string2tokens(input, exp);
    }

    bool bad_booleans()
    {
        string const input{ "#tjunk" };
        vector<Token> exp {
            Token(TokenType::TT_SCAN_ERROR, "Unable to scan input: {#tjunk}"),
        };

        return string2tokens(input, exp);
    }

    bool characters()
    {
        string const input{ "#\\a#\\Z    #\\+#\\ #\\\\" };
        vector<Token> exp {
            Token(TokenType::TT_CHARACTER, "a"),
            Token(TokenType::TT_CHARACTER, "Z"),
            Token(TokenType::TT_CHARACTER, "+"),
            Token(TokenType::TT_CHARACTER, " "),
            Token(TokenType::TT_CHARACTER, "\\"),
        };

        return string2tokens(input, exp);
    }

    bool badcharacter1()
    {
        string const input{ "#\\" };
        vector<Token> exp {
            Token(TokenType::TT_SCAN_ERROR, "Malformed character: {#\\}"),
        };

        return string2tokens(input, exp);
    }

    bool badcharacter2()
    {
        string const input{ "#\\a23" };
        vector<Token> exp {
            Token(TokenType::TT_SCAN_ERROR, "Unable to scan input: {#\\a23}"),
        };

        return string2tokens(input, exp);
    }

    bool emptystring()
    {
        string const input{ "\"\"" };
        vector<Token> exp {
            Token(TokenType::TT_STRING, "")
        };

        return string2tokens(input, exp);
    }

    bool nonemptystring()
    {
        string const input{ "\"Hello, World\"" };
        vector<Token> exp {
            Token(TokenType::TT_STRING, "Hello, World")
        };

        return string2tokens(input, exp);
    }

    bool badstring()
    {
        string const input{ "\"" };
        string const msg{ "End of input in string: {\"}" };
        vector<Token> exp { Token(TokenType::TT_SCAN_ERROR, msg) };

        return string2tokens(input, exp);
    }

    bool integers()
    {
        string const input{ "1 +3 -5" };
        vector<Token> exp {
            Token(TokenType::TT_INTEGER, "1"),
            Token(TokenType::TT_INTEGER, "+3"),
            Token(TokenType::TT_INTEGER, "-5")
        };

        return string2tokens(input, exp);
    }

    bool floats()
    {
        string const input{ "0.0001 +3.2 -5.01" };
        vector<Token> exp {
            Token(TokenType::TT_FLOAT, "0.0001"),
            Token(TokenType::TT_FLOAT, "+3.2"),
            Token(TokenType::TT_FLOAT, "-5.01")
        };

        return string2tokens(input, exp);
    }

    bool falsefloats()
    {
        string const input{ ".2" };
        vector<Token> exp {
            Token(TokenType::TT_DOT, "."),
            Token(TokenType::TT_INTEGER, "2")
        };

        return string2tokens(input, exp);
    }
}

bool tokenizertest()
{
    bool ok { true };

    ok &= emptytest();
    ok &= whitespace();
    ok &= simplecomments();
    ok &= nestedcomments1();
    ok &= nestedcomments2();
    ok &= nestedcommentsbad1();
    ok &= nestedcommentsbad2();
    ok &= nestedcommentsbad3();

    ok &= specials();
    ok &= booleans();
    ok &= bad_booleans();
    ok &= characters();
    ok &= badcharacter1();
    ok &= badcharacter2();
    ok &= emptystring();
    ok &= nonemptystring();
    ok &= badstring();

    ok &= integers();
    ok &= floats();
    ok &= falsefloats();

    return ok;
}
