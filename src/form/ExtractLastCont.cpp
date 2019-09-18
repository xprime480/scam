#include "form/ExtractLastCont.hpp"

#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "expr/SequenceOps.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;

ExtractLastCont::ExtractLastCont(Continuation * cont)
    : Continuation("Extract Last")
    , cont(cont)
{
}

ExtractLastCont * ExtractLastCont::makeInstance(Continuation * cont)
{
    return new ExtractLastCont(cont);
}

void ExtractLastCont::mark()
{
    cont->mark();
}

void ExtractLastCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    ScamEngine & engine = ScamEngine::getEngine();
    if ( isUnhandledError(value) ) {
        engine.handleError(value);
    }
    else {
        if ( isList(value) ) {
            if ( isNull(value) ) {
                ScamValue err =
                    makeError("**InternalError (ExtractLast), "
                              "expected non-empty list");
                err->errorCategory() = internalCategory;
                engine.handleError(value);
            }
            else {
                size_t len = length(value);
                ScamValue result = nthcar(value, len - 1);
                cont->handleValue(result);
            }
        }
        else {
            ScamValue err =
                makeError("**InternalError (ExtractLast), "
                          "expected list, got %{0}",
                          value);
            err->errorCategory() = internalCategory;
            engine.handleError(value);
        }
    }
}
