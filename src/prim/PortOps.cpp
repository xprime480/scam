#include "prim/PortOps.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "port/FixedStringPort.hpp"
#include "port/StringPort.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applyOpenInStr(ScamValue args, Continuation * cont)
{
    StringParameter p0;
    if ( argsToParms(args, "open-input-string", p0) ) {
        string text = asString(p0.value);
        ScamPort * port = new FixedStringPort(text.c_str());
        ScamValue rv = makePort(port);
        cont->handleValue(rv);
    }
}

void scam::applyOpenOutStr(ScamValue args, Continuation * cont)
{
    if ( argsToParms(args, "open-output-string") ) {
        ScamPort * port = new StringPort("", ScamPort::Writeable);
        ScamValue rv = makePort(port);
        rv->setMeta("open-output-string", makeNothing());
        cont->handleValue(rv);
    }
}

void scam::applyGetOutStr(ScamValue args, Continuation * cont)
{
    PortParameter p0;
    if ( argsToParms(args, "get-output-string", p0) ) {
        ScamValue p = p0.value;
        if ( truth(p->hasMeta("open-output-string")) ) {
            ScamPort * port = asPort(p);
            ScamValue rv = port->getContents();
            if ( isString(rv) ) {
                cont->handleValue(rv);
            }
            else {
                ScamValue err =
                    makeError("Internal error, invalid port contents %{0}", rv);
                err->errorCategory() = evalCategory;
                ScamEngine::getEngine().handleError(err);
            }
        }
        else {
            ScamValue err = makeError("Port is not an input string %{0}", p);
            err->errorCategory() = evalCategory;
            ScamEngine::getEngine().handleError(err);
        }
    }
}
