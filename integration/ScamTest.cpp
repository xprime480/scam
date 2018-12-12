#include "ScamTest.hpp"

#include "TestLoader.hpp"
#include "scam.hpp"

#include "ScamException.hpp"

#include <iostream>
#include <vector>

using namespace std;
using namespace scam;
using namespace scam::test;

namespace
{
    int SHOW_NONE = 0x0000;
    int SHOW_PASS = 0x0001;
    int SHOW_FAIL = 0x0002;
    int SHOW_ALL  = 0xFFFF;

    extern string strip_trailing_whitespace(string const & str);
    extern string get_last_lines(string const & str, size_t count);
}

ScamTest::ScamTest(TestLoader & loader)
    : ok(false)
    , skip(false)
    , actual("Test Not Run")
    , linesToKeep(0u)
{
    skip = loader.isSkipSet();
    if ( skip ) {
        return;
    }

    ok = loader.isOK();
    if ( ! ok ) {
        return;
    }

    loader.getComponents(name, input, expected);
    linesToKeep = loader.getLinesToKeep();
    expected = strip_trailing_whitespace(expected);
}

bool ScamTest::isOK() const
{
    return ok;
}

bool ScamTest::isSkipSet() const
{
    return skip;
}

bool ScamTest::isPassed() const
{
    return actual == expected;
}

void ScamTest::run()
{
    input = strip_trailing_whitespace(input);

    try {
        string raw = call_scam(input);
        string stripped = strip_trailing_whitespace(raw);
        actual = get_last_lines(stripped, linesToKeep);
    }
    catch ( ScamException e ) {
        actual = e.getMessage();
    }

    if ( actual == expected ) {
        dopass(SHOW_FAIL);
    }
    else {
        dofail(SHOW_FAIL, actual);
    }
}

bool ScamTest::dopass(int mode) const
{
    if ( mode & SHOW_PASS ) {
        cout << "[PASS] " << name << "\n";
    }
    return true;
}

bool ScamTest::dofail(int mode, string const & actual) const
{
    if ( mode & SHOW_FAIL ) {
        cout << "[FAIL] " << name << "\n";
        cout << "\texpected: \"" << expected << "\"\n";
        cout << "\t  actual: \"" << actual << "\"\n";
    }
    return false;
}

namespace
{
    string strip_trailing_whitespace(string const & str)
    {
        char const * beg = str.c_str();
        char const * end = beg + str.size();

        while ( end >= beg ) {
            char const c = *end;
            if ( !c || isspace(c) || ( '\n' == c ) ) {
                --end;
            }
            else {
                break;
            }
        }

        ++end;
        return string(beg, end);
    }

    string get_last_lines(string const & str, size_t count)
    {
        if ( 0 == count || str.empty() ) {
            return str;
        }

        vector<string> buffer;
        size_t whereami { 0u };
        for ( ;; ) {
            auto pos = str.find('\n', whereami);
            if ( pos == string::npos ) {
                buffer.push_back(str.substr(whereami));
                break;
            }
            else {
                size_t len = pos - whereami;
                buffer.push_back(str.substr(whereami, len));
                whereami = pos + 1;
            }
        }

        if ( buffer.size() <= count ) {
            return str;
        }
        else if ( 1u == count ) {
            return buffer.back();
        }
        else {
            size_t len = buffer.size();
            size_t fr = len - count;
            size_t to = len - 1;

            stringstream s;
            for ( size_t idx = fr ; idx < to ; ++idx ) {
                s << buffer[idx] << "\n";
            }
            s << buffer.back();

            return s.str();
        }

        return str;
    }
}


