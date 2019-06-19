#include "prim/ErrorOps.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "input/ListParser.hpp"
#include "prim/UserHandler.hpp"
#include "prim/WithHandlerCont.hpp"
#include "util/ArgListHelper.hpp"

#include "util/GlobalId.hpp"
#include "util/DebugTrace.hpp"
#include "expr/ValueWriter.hpp"

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
    ArgListHelper helper(args);

    ScamValue obj;
    if ( ! wantObject(name, helper, cont, engine, obj) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    engine->handleError(obj);
}

void scam::applyWithHandler(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name = "with-exception-handler";
    ArgListHelper helper(args);

    ScamValue handler, thunk;
    if ( ! wantApplicable(name, helper, cont, engine, handler, 1) ) {
        return;
    }
    if ( ! wantApplicable(name, helper, cont, engine, thunk, 0) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

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

namespace
{
    ScamValue makeErrorObject(ScamValue args,
                              Continuation * cont,
                              ScamEngine * engine,
                              const char * name)
    {
        ArgListHelper helper(args);
        ScamValue rv = makeNothing();

        string str;
        ScamValue objs;
        if ( ! wantString(name, helper, cont, engine, str) ) {
            return rv;
        }
        if ( ! wantZeroPlus(name, helper, cont, engine, objs, isAnything) ) {
            return rv;
        }
        if ( ! finishArgs(name, helper, cont, engine) ) {
            return rv;
        }

        ScamData::VectorData irritants;
        unsigned len = length(objs);
        for ( unsigned i = 0 ; i < len ; ++i ) {
            irritants.push_back(nthcar(objs, i));
        }

        rv = makeError(str.c_str(), irritants);
        return rv;
    }

    ScamValue fetchErrorObject(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine,
                               const char * name)
    {
        ArgListHelper helper(args);

        ScamValue error = makeNothing();

        if ( ! wantError(name, helper, cont, engine, error) ) {
            return error;
        }
        if ( ! finishArgs(name, helper, cont, engine) ) {
            return error;
        }

        return error;
    }
}
