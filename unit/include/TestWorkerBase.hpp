#if ! defined(TESTWORKERBASE_HPP)
#define TESTWORKERBASE_HPP 1

#include "Worker.hpp"

namespace scam
{
    namespace test_impl
    {
        class TestWorkerBase : public Worker
        {
        protected:
            TestWorkerBase(ScamEngine * engine,
                           size_t * counter,
                           char const * name);

        public:
            void run() override;

        protected:
            size_t * counter;
        };
    }
}

#endif
