#include "prim/InputOps.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/ValueFactory.hpp"
#include "input/CharStreamTokenizer.hpp"
#include "input/PortCharStream.hpp"
#include "input/ScamParser.hpp"
#include "port/ScamPort.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applyRead(ScamValue args, Continuation * cont)
{
    PortParameter p0;
    if ( argsToParms(args, "read", p0) ) {
        ScamValue port = p0.value;

        port->portValue()->rollback();
        PortCharStream stream(port);
        CharStreamTokenizer tokenizer(stream);
        ScamParser parser(tokenizer);

        ScamValue rv = parser.parseExpr();
        cont->handleValue(rv);
    }
}

void scam::applyEofObject(ScamValue args,
                          Continuation * cont)
{
    if ( argsToParms(args, "eof-object") ) {
        cont->handleValue(makeEof());
    };
}


