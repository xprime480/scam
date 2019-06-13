#include "OneShotWorker.hpp"

using namespace scam;
using namespace scam::test_impl;

OneShotWorker::OneShotWorker(ScamEngine * engine, size_t * counter)
    : TestWorkerBase(engine, counter, "OneShotWorker")
{
}

OneShotWorker *
OneShotWorker::makeInstance(ScamEngine * engine, size_t * counter)
{
    return new OneShotWorker(engine, counter);
}

void OneShotWorker::run()
{
    TestWorkerBase::run();
}
