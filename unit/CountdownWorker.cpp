
#include "CountdownWorker.hpp"

#include "WorkQueue.hpp"

using namespace scam;
using namespace scam::test_impl;

CountdownWorker::CountdownWorker(size_t n,
                                 size_t * counter,
                                 WorkQueue & queue)
    : TestWorkerBase(counter, "CountDown")
    , n(n)
    , queue(queue)
{
}

CountdownWorker * CountdownWorker::makeInstance(size_t n,
                                                size_t * counter,
                                                WorkQueue & queue)
{
    return new CountdownWorker(n, counter, queue);
}

void CountdownWorker::run()
{
    TestWorkerBase::run();
    if ( 0 == n ) {
        //            cout << "go!!\n";
    }
    else {
        //            cout << n << "... ";

      Worker * next =
          standardMemoryManager.make<CountdownWorker>(n-1,
                                                      counter,
                                                      queue);
      queue.put(next);
    }
}
