
#include "OneShotWorker.hpp"

using namespace scam;
using namespace scam::test_impl;

OneShotWorker::OneShotWorker(size_t * counter)
    : TestWorkerBase(counter, "OneShotWorker")
{
}

OneShotWorker * OneShotWorker::makeInstance(size_t * counter)
{
    return new OneShotWorker(counter);
}

void OneShotWorker::run()
{
    TestWorkerBase::run();
}
