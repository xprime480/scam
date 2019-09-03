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

            CountdownWorker(size_t n, size_t * counter, WorkQueue & queue);

            static CountdownWorker *
            makeInstance(size_t n, size_t * counter, WorkQueue & queue);

        public:
            size_t n;
            WorkQueue & queue;

            // void mark() override; fixme?
            void run() override;
        };
    }
}

#endif
