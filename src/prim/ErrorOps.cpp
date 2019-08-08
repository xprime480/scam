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
    extern ScamValue makeErrorObject(ScamValue args,
                                     Continuation * cont,
                                     ScamEngine * engine,
                                     const char * name);

    extern ScamValue fetchErrorObject(ScamValue args,
                                      Continuation * cont,
                                      ScamEngine * engine,
                                      const char * name);

    extern void checkErrorCategory(ScamValue args,
                                   Continuation * cont,
                                   ScamEngine * engine,
                                   const char * name,
                                   ScamValue target);
}

void scam::applyMakeError(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name = "make-error";
    ScamValue error = makeErrorObject(args, cont, engine, name);
    if ( ! isNothing(error) ) {
        error->errorHandled() = true;
        cont->handleValue(error);
    }
}

void scam::applyError(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    static const char * name = "error";
    ScamValue error = makeErrorObject(args, cont, engine, name);
    if ( ! isNothing(error) ) {
        engine->handleError(error);
    }
}

void scam::applyRaise(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    static const char * name = "raise";

    ObjectParameter pObj;
    if ( argsToParms(args, engine, name, pObj) ) {
        engine->handleError(pObj.value);
    }
}

void scam::applyWithHandler(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name = "with-exception-handler";

    ApplicableParameter pHandler;
    ApplicableParameter pThunk;
    if ( argsToParms(args, engine, name, pHandler, pThunk) ) {
        ScamValue handler = pHandler.value;
        ScamValue thunk   = pThunk.value;

        Continuation * newCont =
            standardMemoryManager.make<WithHandlerCont>(cont, engine);

        Handler * wrapper =
            standardMemoryManager.make<UserHandler>(handler,
                                                    cont,
                                                    env(handler),
                                                    engine);

        engine->pushHandler(wrapper);
        apply(thunk, makeNull(), newCont, env(thunk), engine);
    }
}

void scam::applyErrorMessage(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
{
    static const char * name = "error-object-message";

    ScamValue error = fetchErrorObject(args, cont, engine, name);
    if ( isError(error) ) {
        const string & message = error->errorMessage();
        ScamValue rv = makeString(message);
        cont->handleValue(rv);
    }
}

void scam::applyErrorIrritant(ScamValue args,
                              Continuation * cont,
                              ScamEngine * engine)
{
    static const char * name = "error-object-irritants";

    ScamValue error = fetchErrorObject(args, cont, engine, name);
    if ( isError(error) ) {
        const ScamData::VectorData & irritants = error->errorIrritants();
        ScamValue rv = makeList(irritants);
        cont->handleValue(rv);
    }
}

void scam::applyReadErrorP(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name = "read-error?";
    checkErrorCategory(args, cont, engine, name, readCategory);
}

void scam::applyFileErrorP(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name = "file-error?";
    checkErrorCategory(args, cont, engine, name, fileCategory);
}

void scam::applyErrorCat(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine)
{
    static const char * name = "error-type";

    ScamValue error = fetchErrorObject(args, cont, engine, name);
    if ( isError(error) ) {
        ScamValue rv = error->errorCategory();
        cont->handleValue(rv);
    }
}

namespace
{
    ScamValue makeErrorObject(ScamValue args,
                              Continuation * cont,
                              ScamEngine * engine,
                              const char * name)
    {
        ScamValue rv = makeNothing();

        KeywordParameter  pCat;
        OptionalParameter p0(pCat);
        StringParameter   p1;
        ObjectParameter   pTemp;
        CountedParameter  p2(pTemp);
        if ( argsToParms(args, engine, name, p0, p1, p2) ) {
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

    ScamValue fetchErrorObject(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine,
                               const char * name)
    {
        ScamValue rv = makeNothing();

        ErrorParameter p0;
        if ( argsToParms(args, engine, name, p0) ) {
            rv = p0.value;
        }

        return rv;
    }

    void checkErrorCategory(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine,
                            const char * name,
                            ScamValue target)
    {
        ErrorParameter p0;
        if ( argsToParms(args, engine, name, p0) ) {
            ScamValue obj = p0.value;
            ScamValue cat = obj->errorCategory();
            bool matched = equals(cat, target);
            cont->handleValue(makeBoolean(matched));
        }
    }
}
