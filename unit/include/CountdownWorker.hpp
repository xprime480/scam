#if ! defined(COUNTDOWNWORKER_HPP)
#define COUNTDOWNWORKER_HPP

#include "TestWorkerBase.hpp"

namespace scam
{
    class MemoryManager;
    class WorkQueue;

    namespace test_impl
    {
        class CountdownWorker : public TestWorkerBase
        {
        private:
            friend class scam::MemoryManager;

            CountdownWorker(ScamEngine * engine,
                            size_t n,
                            size_t * counter,
                            WorkQueue & queue);

            static CountdownWorker * makeInstance(ScamEngine * engine,
                                                  size_t n,
                                                  size_t * counter,
                                                  WorkQueue & queue);

        public:
            size_t n;
            WorkQueue & queue;

            void run() override;
        };
    }
}

#endif
