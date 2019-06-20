#include "prim/PortOps.hpp"

#include "Continuation.hpp"
#include "port/FixedStringPort.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

void scam::applyOpenInStr(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "open-input-string" };
    ArgListHelper helper(args);

    string value;
    if ( ! wantString(name, helper, cont, engine, value) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    ScamPort * port = new FixedStringPort(value.c_str());
    ScamValue rv = makePort(port);
    cont->handleValue(rv);
}
