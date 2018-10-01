
#include "tests.hpp"

#include <functional>

using namespace std;

class UnitTestMain
{
public:
    UnitTestMain(int argc, char ** argv)
        : count(0)
        , pass(0)
    {
    }

    void run()
    {
        test(trampolinetest);
        test(tokenizertest);
        test(parsertest);
    }

    int rc() const
    {
        return (count == pass) ? 0 : 1;
    }


private:
    size_t count;
    size_t pass;

    void test(function<bool()> t)
    {
        ++count;
        if ( t() ) {
            ++pass;
        }
    }
};

int main(int argc, char ** argv)
{
    UnitTestMain unit(argc, argv);
    unit.run();
    return unit.rc();
}
