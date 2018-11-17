
#include "scam.hpp"

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
");

    bool extend_for_testing(ScamEngine & engine)
    {
        engine.pushFrame();
        EvalString helper(&engine, testforms);
        ExprHandle status = helper.getLast();
        return ! ( status->isNull() || status->error() );
    }
}

string call_scam(string const & input)
{
    ScamEngine engine(true);
    bool check = extend_for_testing(engine);
    if ( ! check ) {
        return "** Internal Test Error initializing test environment";
    }

    EvalString helper(&engine, input);
    vector<ExprHandle> values;
    helper.getAll(values, false);

    stringstream s;
    for ( auto v : values ) {
        s << v->toString() << "\n";
    }
    return s.str();
}

