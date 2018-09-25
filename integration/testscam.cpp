#include "TestManager.hpp"

using namespace scam::test;

int main(int argc, char ** argv)
{
    TestManager manager;
    return manager.runsuite(argc, argv);
}

