#include "scam.hpp"

#include "Accumulator.hpp"
#include "Extractor.hpp"
#include "TestHandler.hpp"
#include "ScamEngine.hpp"
#include "input/StringTokenizer.hpp"
#include "util/ReadEvalString.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static string const testforms("\
(define add +)\
(define sub -)\
(define two 2)\
#t\
");

    bool extend_for_testing(ScamEngine & engine)
    {
        engine.reset(true);
        engine.pushFrame();
        ReadEvalString helper(&engine, testforms);
        ScamValue status = helper.run();
        if ( isNothing(status) || isError(status) ) {
            return false;
        }
        else {
            return true;
        }
    }
}

string call_scam(string const & input)
{
    ScamEngine engine;
    bool check = extend_for_testing(engine);
    if ( ! check ) {
        return "** Internal Test Error initializing test environment";
    }

    Accumulator * accumulator =
        standardMemoryManager.make<Accumulator>(&engine);
    engine.setCont(accumulator);

    TestHandler handler(accumulator);
    engine.pushHandler(&handler);

    ReadEvalString helper(&engine, input);
    helper.run();

    engine.popHandler();

    string rv = accumulator->getResult();
    return rv;
}
