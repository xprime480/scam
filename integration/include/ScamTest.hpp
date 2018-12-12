#if ! defined(SCHEMETEST_H)
#define SCHEMETEST_H 1

#include <string>

namespace scam
{
    namespace test
    {
        class TestLoader;

        class ScamTest
        {
        public:
            ScamTest(TestLoader & loader);

            bool isOK() const;
            bool isSkipSet() const;
            bool isPassed() const;

            void run();

        private:
            bool ok;
            bool skip;
            bool pass;

            std::string name;
            std::string input;
            std::string expected;
            std::string actual;

            size_t linesToKeep;

            bool dopass(int mode) const;
            bool dofail(int mode, std::string const & actual) const;
        };
    }
}


#endif


