
#include "scam.hpp"

#include "output/OutputHandlerBuffered.hpp"
#include "ScamEngine.hpp"
#include "input/StringTokenizer.hpp"

using namespace scam;

namespace
{
    static std::string const testforms("\
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

        void handleResult(std::string const & result) {}

        void handleError(std::string const & error) { ok = false; }

        void handleTrace(std::string const & msg) {}

        operator bool() const
        {
            return ok;
        }

    private:
        bool ok;
    };

}

std::string call_scam(std::string const & input)
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

    std::string const & rv = output.get();
    return rv;
}

