#include "ScamTest.hpp"

#include "TestLoader.hpp"
#include "scam.hpp"

#include <iostream>

using namespace scam;
using namespace scam::test;

namespace
{
    int SHOW_NONE = 0x0000;
    int SHOW_PASS = 0x0001;
    int SHOW_FAIL = 0x0002;
    int SHOW_ALL  = 0xFFFF;

    std::string strip_trailing_whitespace(std::string const & str);
}

ScamTest::ScamTest(TestLoader & loader)
    : ok(false)
    , skip(false)
    , actual("Test Not Run")
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
    actual = strip_trailing_whitespace(call_scam(input));
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
        std::cout << "[PASS] " << name << "\n";
    }
    return true;
}

bool ScamTest::dofail(int mode, std::string const & actual) const
{
    if ( mode & SHOW_FAIL ) {
        std::cout << "[FAIL] " << name << "\n";
        std::cout << "\texpected: \"" << expected << "\"\n";
        std::cout << "\t  actual: \"" << actual << "\"\n";
    }
    return false;
}

namespace
{
    std::string strip_trailing_whitespace(std::string const & str)
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
        return std::string(beg, end);
    }
}


