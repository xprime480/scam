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

string call_scam(string const & input)
{
    ScamEngine engine;
    engine.reset(true);

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
