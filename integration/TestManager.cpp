
#include "TestManager.hpp"

#include "TestLoader.hpp"
#include "ScamTest.hpp"

#include <iostream>

using namespace scam;
using namespace scam::test;

TestManager::TestManager()
    : runCount(0)
    , skipCount(0)
    , passCount(0)
    , badCount(0)
{
}

int TestManager::runsuite(int argc, char ** argv)
{
    for ( int idx = 1 ; idx < argc ; ++idx ) {
        runtest(argv[idx]);
    }

    std::cout << passCount << " passed of " << runCount << " tests\n";
    std::cout << badCount << " incorrectly formatted\n";
    std::cout << skipCount << " skipped\n";

    int rv = 0;
    rv |= passCount == runCount ? 0 : 1;
    rv |= badCount == 0 ? 0 : 2;
    return rv;
}

void TestManager::runtest(char const * script)
{
    TestLoader loader(script);
    ScamTest   test(loader);

    if ( test.isSkipSet() ) {
        std::cout << script << " is skipped\n";
        ++skipCount;
        return;
    }

    if ( ! test.isOK() ) {
        std::cout << script << " is bad\n";
        ++badCount;
        return;
    }

    ++runCount;
    test.run();
    if ( test.isPassed() ) {
        ++passCount;
    }
    else {
        std::cout << script << " failed\n";
    }
}


