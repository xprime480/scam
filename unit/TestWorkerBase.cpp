#include "TestWorkerBase.hpp"

using namespace scam;
using namespace scam::test_impl;

TestWorkerBase::TestWorkerBase(ScamEngine * engine,
                               size_t * counter,
                               char const *name)
    : Worker(name, engine)
    , counter(counter)
{
}

void TestWorkerBase::run()
{
    Worker::run();
    ++*counter;
}
