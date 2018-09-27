
#include "scam.hpp"

#include "output/OutputHandlerBuffered.hpp"
#include "ScamEngine.hpp"
#include "input/StringTokenizer.hpp"

using namespace scam;
using namespace std;

namespace
{
    static string const testforms("\
(define add +)\
(define sub -)\
(define two 2)\
");

    class OutputHandlerCheck : public OutputHandler
    {
    public:
        OutputHandlerCheck()
            : OutputHandler()
            , ok(true)
        {
        }

        void handleResult(string const & result) {}

        void handleError(string const & error) { ok = false; }

        void handleTrace(string const & msg) {}

        operator bool() const
        {
            return ok;
        }

    private:
        bool ok;
    };

}

string call_scam(string const & input)
{
    ScamEngine engine;

    StringTokenizer test(testforms);
    OutputHandlerCheck check;
    engine.extend("**test**", test, check);
    if ( ! check ) {
        return "** Internal Test Error initializing test environment";
    }

    StringTokenizer tokenizer(input);
    OutputHandlerBuffered output;

    engine.repl(tokenizer, output);

    string const & rv = output.get();
    return rv;
}

