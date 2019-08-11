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

void scam::applyOpenOutStr(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    if ( argsToParms(args, engine, "open-output-string") ) {
        ScamPort * port = new StringPort("", ScamPort::Writeable);
        ScamValue rv = makePort(port);
        rv->setMeta("open-output-string", makeNothing());
        cont->handleValue(rv);
    }
}

void scam::applyGetOutStr(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    PortParameter p0;
    if ( argsToParms(args, engine, "get-output-string", p0) ) {
        ScamValue p = p0.value;
        if ( p->hasMeta("open-output-string") ) {
            ScamPort * port = asPort(p);
            ScamValue rv = port->getContents();
            if ( isString(rv) ) {
                cont->handleValue(rv);
            }
            else {
                ScamValue err =
                    makeError("Internal error, invalid port contents %{0}", rv);
                err->errorCategory() = evalCategory;
                engine->handleError(err);
            }
        }
        else {
            ScamValue err = makeError("Port is not an input string %{0}", p);
            err->errorCategory() = evalCategory;
            engine->handleError(err);
        }
    }
}
