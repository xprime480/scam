#include "prim/ErrorOps.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/ValueFactory.hpp"
#include "input/ScamParser.hpp"
#include "prim/Load.hpp"
#include "prim/UserHandler.hpp"
#include "prim/WithHandlerCont.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern ScamValue
    makeErrorObject(ScamValue args, Continuation * cont, const char * name);

    extern ScamValue
    fetchErrorObject(ScamValue args, Continuation * cont, const char * name);

    extern void
    checkErrorCategory(ScamValue args,
                       Continuation * cont,
                       const char * name,
                       ScamValue target);
}

void scam::applyMakeError(ScamValue args, Continuation * cont)
{
    static const char * name = "make-error";
    ScamValue error = makeErrorObject(args, cont, name);
    if ( ! isNothing(error) ) {
        error->errorHandled() = true;
        cont->handleValue(error);
    }
}

void scam::applyError(ScamValue args, Continuation * cont)
{
    static const char * name = "error";
    ScamValue error = makeErrorObject(args, cont, name);
    if ( ! isNothing(error) ) {
        ScamEngine::getEngine().handleError(error);
    }
}

void scam::applyRaise(ScamValue args, Continuation * cont)
{
    static const char * name = "raise";

    ObjectParameter pObj;
    if ( argsToParms(args, name, pObj) ) {
        ScamEngine::getEngine().handleError(pObj.value);
    }
}

void scam::applyWithHandler(ScamValue args, Continuation * cont)
{
    static const char * name = "with-exception-handler";

    ApplicableParameter pHandler;
    ApplicableParameter pThunk;
    if ( argsToParms(args, name, pHandler, pThunk) ) {
        ScamValue handler = pHandler.value;
        ScamValue thunk   = pThunk.value;

        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Continuation * newCont = mm.make<WithHandlerCont>(cont);
        Handler * wrapper = mm.make<UserHandler>(handler, cont, env(handler));

        ScamEngine::getEngine().pushHandler(wrapper);
        apply(thunk, makeNull(), newCont, env(thunk));
    }
}

void scam::applyErrorMessage(ScamValue args, Continuation * cont)
{
    static const char * name = "error-object-message";

    ScamValue error = fetchErrorObject(args, cont, name);
    if ( isError(error) ) {
        const string & message = error->errorMessage();
        ScamValue rv = makeString(message);
        cont->handleValue(rv);
    }
}

void scam::applyErrorIrritant(ScamValue args, Continuation * cont)
{
    static const char * name = "error-object-irritants";

    ScamValue error = fetchErrorObject(args, cont, name);
    if ( isError(error) ) {
        const ScamData::VectorData & irritants = error->errorIrritants();
        ScamValue rv = makeList(irritants);
        cont->handleValue(rv);
    }
}

void scam::applyReadErrorP(ScamValue args, Continuation * cont)
{
    static const char * name = "read-error?";
    checkErrorCategory(args, cont, name, readCategory);
}

void scam::applyFileErrorP(ScamValue args, Continuation * cont)
{
    static const char * name = "file-error?";
    checkErrorCategory(args, cont, name, fileCategory);
}

void scam::applyErrorCat(ScamValue args, Continuation * cont)
{
    static const char * name = "error-category";

    ScamValue error = fetchErrorObject(args, cont, name);
    if ( isError(error) ) {
        ScamValue rv = error->errorCategory();
        cont->handleValue(rv);
    }
}

void scam::applyError2String(ScamValue args, Continuation * cont)
{
    static const char * name = "error->string";

    ScamValue error = fetchErrorObject(args, cont, name);
    if ( isError(error) ) {
        ScamValue rv = makeString(writeValue(error));
        cont->handleValue(rv);
    }
}

namespace
{
    ScamValue
    makeErrorObject(ScamValue args, Continuation * cont, const char * name)
    {
        ScamValue rv = makeNothing();

        KeywordParameter  pCat;
        OptionalParameter p0(pCat);
        StringParameter   p1;
        ObjectParameter   pTemp;
        CountedParameter  p2(pTemp);
        if ( argsToParms(args, name, p0, p1, p2) ) {
            ScamValue cat = userCategory;
            if ( p0.found ) {
                cat = p0.value;
            }
            string str     = asString(p1.value);
            ScamValue objs = p2.value;

            ScamData::VectorData irritants;
            unsigned len = length(objs);
            for ( unsigned i = 0 ; i < len ; ++i ) {
                irritants.push_back(nthcar(objs, i));
            }

            rv = makeError(str.c_str(), irritants);
            rv->errorCategory() = cat;
        }

        return rv;
    }

    ScamValue
    fetchErrorObject(ScamValue args, Continuation * cont, const char * name)
    {
        ScamValue rv = makeNothing();

        ErrorParameter p0;
        if ( argsToParms(args, name, p0) ) {
            rv = p0.value;
        }

        return rv;
    }

    void checkErrorCategory(ScamValue args,
                            Continuation * cont,
                            const char * name,
                            ScamValue target)
    {
        ErrorParameter p0;
        if ( argsToParms(args, name, p0) ) {
            ScamValue obj = p0.value;
            ScamValue cat = obj->errorCategory();
            bool matched = equals(cat, target);
            cont->handleValue(makeBoolean(matched));
        }
    }
}
