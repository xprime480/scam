#include "prim/OutputOps.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "port/ScamPort.hpp"
#include "util/Parameter.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    void writeToPort(ScamValue value, ScamValue port);
    ScamValue paramToPort(const OptionalParameter & p, ScamEngine * engine);
}

void scam::applyDisplay(ScamValue args,
                        Continuation * cont,
                        ScamEngine * engine)
{
    ObjectParameter   p0;
    PortParameter     pPort;
    OptionalParameter p1(pPort);

    if ( argsToParms(args, engine, "display", p0, p1) ) {
        ScamValue obj = p0.value;
        ScamValue port = paramToPort(p1, engine);
        writeToPort(obj, port);
        cont->handleValue(makeNothing());
    }
}

void scam::applyNewline(ScamValue args,
                        Continuation * cont,
                        ScamEngine * engine)
{
    PortParameter     pPort;
    OptionalParameter p0(pPort);
    if ( argsToParms(args, engine, "newline", p0) ) {
        static const ScamValue newline = makeString("\n", false);
        ScamValue port = paramToPort(p0, engine);
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

    ScamValue paramToPort(const OptionalParameter & p, ScamEngine * engine)
    {
        ScamValue port =
            p.found ? p.value : engine->eval(makeSymbol("**cerr**"));
        return port;
    }
}
