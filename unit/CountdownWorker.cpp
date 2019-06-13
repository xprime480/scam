
#include "CountdownWorker.hpp"

#include "WorkQueue.hpp"

using namespace scam;
using namespace scam::test_impl;

CountdownWorker::CountdownWorker(ScamEngine * engine,
                                 size_t n,
                                 size_t * counter,
                                 WorkQueue & queue)
    : TestWorkerBase(engine, counter, "CountDown")
    , n(n)
    , queue(queue)
{
}

CountdownWorker * CountdownWorker::makeInstance(ScamEngine * engine,
                                                size_t n,
                                                size_t * counter,
                                                WorkQueue & queue)
{
    return new CountdownWorker(engine, n, counter, queue);
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
          standardMemoryManager.make<CountdownWorker>(engine,
                                                      n-1,
                                                      counter,
                                                      queue);
      queue.put(next);
    }
}
