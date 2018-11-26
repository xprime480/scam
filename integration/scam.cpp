
#include "scam.hpp"

#include "Accumulator.hpp"
#include "Extractor.hpp"
#include "ScamEngine.hpp"
#include "input/StringTokenizer.hpp"
#include "util/EvalString.hpp"

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
        EvalString helper(&engine, testforms);
        ExprHandle status = helper.run();
        return ! ( status->isNull() || status->error() );
    }
}

string call_scam(string const & input)
{
    ScamEngine engine;
    bool check = extend_for_testing(engine);
    if ( ! check ) {
        return "** Internal Test Error initializing test environment";
    }

    shared_ptr<Accumulator> accumulator = make_shared<Accumulator>();
    engine.setCont(accumulator);
    EvalString helper(&engine, input);
    helper.run();

    return accumulator->getResult();
}

