#include "prim/ReadOps.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "input/CharStreamTokenizer.hpp"
#include "input/PortCharStream.hpp"
#include "input/ScamParser.hpp"
#include "port/ScamPort.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

void scam::applyRead(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * name { "read" };
    ArgListHelper helper(args);

    ScamValue value;
    if ( ! wantPort(name, helper, cont, engine, value) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    value->portValue()->rollback();
    PortCharStream stream(value);
    CharStreamTokenizer tokenizer(stream);
    ScamParser parser(tokenizer);
    ScamValue rv = parser.parseExpr();

    if ( isError(rv) ) {
        engine->handleError(rv);
    }
    else {
        cont->handleValue(rv);
    }
}
