
#include "TestWorkerBase.hpp"

using namespace scam;
using namespace scam::test_impl;

TestWorkerBase::TestWorkerBase(size_t * counter, char const *name)
    : Worker(name)
    , counter(counter)
{
}

void TestWorkerBase::run()
{
    Worker::run();
    ++*counter;
}
