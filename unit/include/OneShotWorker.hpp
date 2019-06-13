#if ! defined(ONESHOTWORKER_HPP)
#define ONESHOTWORKER_HPP

#include "TestWorkerBase.hpp"

namespace scam
{
    class MemoryManager;

    namespace test_impl
    {
        class OneShotWorker : public TestWorkerBase
        {
        private:
            friend class scam::MemoryManager;
            OneShotWorker(ScamEngine * engine, size_t * counter);

            static OneShotWorker *
            makeInstance(ScamEngine * engine, size_t * counter);

        public:
            void run() override;
        };
    }
}

#endif
