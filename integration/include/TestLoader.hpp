#if ! defined(TESTLOADER_H)
#define TESTLOADER_H 1

#include <fstream>
#include <sstream>
#include <string>

namespace scam
{
    namespace test
    {
        class TestLoader
        {
        public:
            TestLoader(char const * script);

            bool isSkipSet() const;
            bool isOK() const;
            void getComponents(std::string & name,
                               std::string & input,
                               std::string & expected) const;

            size_t getLinesToKeep() const;

        private:
            enum Mode { NONE, SKIP, NAME, INPUT, RESULT };

            std::ifstream testspec;

            Mode mode;

            std::stringstream name;
            std::stringstream input;
            std::stringstream expected;

            bool skip;
            bool ok;

            size_t linesToKeep;

            bool getNextLine(std::string & line);
            bool setMode(std::string const & line);
            void handleLine(std::string const & line);
        };
    }
}

#endif
