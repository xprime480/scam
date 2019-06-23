#include "prim/PortOps.hpp"

#include "Continuation.hpp"
#include "expr/ScamToInternal.hpp"
#include "port/FixedStringPort.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applyOpenInStr(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    StringParameter p0;
    if ( argsToParms(args, engine, "open-input-string", p0) ) {
        string text = asString(p0.value);
        ScamPort * port = new FixedStringPort(text.c_str());
        ScamValue rv = makePort(port);
        cont->handleValue(rv);
    }
}
