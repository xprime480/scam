#if ! defined(TESTMANAGER_H)
#define TESTMANAGER_H 1

#include <cstdlib>

namespace scam
{
    namespace test
    {
        class TestManager
        {
        public:
            TestManager();

            int runsuite(int argc, char ** argv);

        private:
            size_t runCount;
            size_t skipCount;
            size_t passCount;
            size_t badCount;

            void runtest(char const * script);
        };
    }
}

#endif


