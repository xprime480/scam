#include "prim/OutputOps.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "port/ScamPort.hpp"
#include "util/Parameter.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    void writeToPort(ScamValue value, ScamValue port);
    ScamValue paramToPort(const OptionalParameter & p);
}

void scam::applyDisplay(ScamValue args, Continuation * cont)
{
    ObjectParameter   p0;
    PortParameter     pPort;
    OptionalParameter p1(pPort);

    if ( argsToParms(args, "display", p0, p1) ) {
        ScamValue obj = p0.value;
        ScamValue port = paramToPort(p1);
        writeToPort(obj, port);
        cont->handleValue(makeNothing());
    }
}

void scam::applyNewline(ScamValue args, Continuation * cont)
{
    PortParameter     pPort;
    OptionalParameter p0(pPort);
    if ( argsToParms(args, "newline", p0) ) {
        static const ScamValue newline = makeString("\n", false);
        ScamValue port = paramToPort(p0);
        writeToPort(newline, port);
        cont->handleValue(makeNothing());
    }
}

namespace
{
    void writeToPort(ScamValue value, ScamValue port)
    {
        stringstream s;

        if ( isString(value) ) {
            s << asString(value);
        }
        else {
            s << writeValue(value);
        }

        const string text = s.str();
        asPort(port)->put(text.c_str(), text.size());
    }

    ScamValue paramToPort(const OptionalParameter & p)
    {
        ScamEngine & e = ScamEngine::getEngine();
        ScamValue port = p.found ? p.value : e.eval(makeSymbol("**cerr**"));
        return port;
    }
}
