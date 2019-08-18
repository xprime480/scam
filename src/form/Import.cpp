#include "form/AllSpecialForms.hpp"

#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/FileUtils.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern ScamValue importImportSet(ScamValue arg, ScamEngine * engine);

    extern ScamValue importLib(ScamValue symbol, ScamEngine * engine);
    extern ScamValue importOnly(ScamValue args, ScamEngine * engine);
    extern ScamValue importExcept(ScamValue args, ScamEngine * engine);
    extern ScamValue importPrefix(ScamValue args, ScamEngine * engine);
    extern ScamValue importRename(ScamValue args, ScamEngine * engine);

    extern ScamValue
    importCommon(ScamValue args, ScamEngine * engine, const char * name);

    extern ScamValue validateKey(Env * env, ScamValue key, const char * name);
    extern string findImportLib(ScamValue lib);

    /*** error messages ***/

    extern ScamValue libraryNotFound(ScamValue symbol);
    extern ScamValue unknownImportDirective(ScamValue arg);
    extern ScamValue importError(ScamValue lib, ScamValue error);
    extern ScamValue badPrefix(ScamValue args);
    extern ScamValue insufficientParameters(ScamValue args, const char * where);
    extern ScamValue badSymbol(ScamValue arg, const char * where);
    extern ScamValue missingSymbol(ScamValue arg, const char * where);
}

void scam::applyImport(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    ScamValue result = importFromSpec(args, engine);
    if ( isUnhandledError(result) ) {
        engine->handleError(result);
    }
}

ScamValue scam::importFromSpec(ScamValue args, ScamEngine * engine)
{
    engine->setFrame(engine->getFrame()->extend());
    ScamValue rv = makeEnv(engine->getFrame());

    while ( ! isNull(args) ) {
        ScamValue arg0 = getCar(args);
        args = getCdr(args);

        ScamValue result = importImportSet(arg0, engine);
        if ( isEnv(result) ) {
            Env * env = asEnv(result);
            engine->getFrame()->merge(env);
        }
        else {
            engine->setFrame(engine->getFrame()->getParent());
            rv = result;
        }
    }

    return rv;
}

namespace
{
    ScamValue importImportSet(ScamValue arg, ScamEngine * engine)
    {
        engine->setFrame(engine->getFrame()->extend());
        ScamValue rv;

        if ( isSymbol(arg) ) {
            rv = importLib(arg, engine);
        }

        else if ( isPair(arg) ) {
            ScamValue directive = getCar(arg);
            ScamValue rest      = getCdr(arg);

            if ( isSymbol(directive) ) {
                const string & name = directive->stringValue();
                if ( name == "only" ) {
                    rv = importOnly(rest, engine);
                }

                else if ( name == "except" ) {
                    rv = importExcept(rest, engine);
                }

                else if ( name == "prefix" ) {
                    rv = importPrefix(rest, engine);
                }

                else if ( name == "rename" ) {
                    rv = importRename(rest, engine);
                }

                else {
                    rv = unknownImportDirective(arg);
                }
            }
            else {
                rv = unknownImportDirective(arg);
            }
        }

        else {
            rv = unknownImportDirective(arg);
        }

        engine->setFrame(engine->getFrame()->getParent());
        if ( isEnv(rv) ) {
            Env * env = engine->getFrame();
            env->merge(asEnv(rv));
            rv = makeEnv(env);
        }

        return rv;
    }

    ScamValue importLib(ScamValue symbol, ScamEngine * engine)
    {
        string fileToLoad = findImportLib(symbol);
        if ( fileToLoad.empty() ) {
            return libraryNotFound(symbol);
        }

        ScamValue rv = loadEvalFile(fileToLoad, engine);
        if ( isUnhandledError(rv) ) {
            return importError(symbol, rv);
        }

        return makeEnv(engine->getFrame());
    }

    ScamValue importOnly(ScamValue args, ScamEngine * engine)
    {
        ScamValue result = importCommon(args, engine, "only");
        if ( ! isEnv(result) ) {
            return result;
        }
        Env * temp = asEnv(result);

        ScamValue rest = getCdr(args);
        Env * base = engine->getFrame();
        while ( isPair(rest) ) {
            ScamValue arg0 = getCar(rest);
            rest           = getCdr(rest);

            ScamValue result = validateKey(temp, arg0, "only");
            if ( isUnhandledError(result)  ) {
                return result;
            }

            base->put(arg0, temp->get(arg0));
        }

        return makeEnv(base);
    }

    ScamValue importExcept(ScamValue args, ScamEngine * engine)
    {
        ScamValue result = importCommon(args, engine, "except");
        if ( ! isEnv(result) ) {
            return result;
        }
        Env * temp = asEnv(result);

        ScamValue rest = getCdr(args);
        Env * base = engine->getFrame();
        while ( isPair(rest) ) {
            ScamValue arg0 = getCar(rest);
            rest           = getCdr(rest);

            ScamValue result = validateKey(temp, arg0, "except");
            if ( isUnhandledError(result)  ) {
                return result;
            }

            temp->remove(arg0);
        }

        ScamValue rv = base->merge(temp);
        if ( isUnhandledError(rv) ) {
            return rv;
        }

        return makeEnv(base);
    }

    ScamValue importPrefix(ScamValue args, ScamEngine * engine)
    {
        ScamValue rest = getCdr(args);
        ScamValue p    = getCar(rest);
        if ( ( ! isSymbol(p)) || ( ! isNull(getCdr(rest))) ) {
            return badPrefix(args);
        }
        const string & prefix = p->stringValue();

        ScamValue result = importCommon(args, engine, "prefix");
        if ( ! isEnv(result) ) {
            return result;
        }

        Env * temp = asEnv(result);
        Env * base = engine->getFrame();
        set<string> keys;
        temp->getKeys(keys);
        for ( const auto k : keys ) {
            stringstream s;
            s << prefix << k;
            ScamValue key = makeSymbol(s.str());
            base->put(key, temp->get(makeSymbol(k)));
        }

        return makeEnv(base);
    }

    ScamValue importRename(ScamValue args, ScamEngine * engine)
    {
        ScamValue result = importCommon(args, engine, "rename");
        if ( ! isEnv(result) ) {
            return result;
        }
        Env * temp = asEnv(result);

        ScamValue rest = getCdr(args);
        Env * base = engine->getFrame();
        while ( isPair(rest) ) {
            ScamValue arg0 = getCar(rest);
            rest           = getCdr(rest);

            SymbolParameter p0;
            SymbolParameter p1;
            ScamValue test = argsToParmsMsg(arg0, p0, p1);
            if ( isUnhandledError(test)  ) {
                return test;
            }

            ScamValue oldName = p0.value;
            ScamValue result = validateKey(temp, oldName, "rename");
            if ( isUnhandledError(result)  ) {
                return result;
            }

            ScamValue newName = p1.value;
            ScamValue value   = temp->get(oldName);
            temp->remove(oldName);
            temp->put(newName, value);
        }

        base->merge(temp);
        return makeEnv(base);
    }

    ScamValue
    importCommon(ScamValue args, ScamEngine * engine, const char * name)
    {
        engine->setFrame(engine->getFrame()->extend());
        ScamValue rv = makeNothing();

        if ( ! isPair(args) ) {
            rv = insufficientParameters(args, name);
        }
        else {
            ScamValue arg = getCar(args);
            rv = importImportSet(arg, engine);
            engine->setFrame(engine->getFrame()->getParent());
        }

        return rv;
    }

    ScamValue validateKey(Env * env, ScamValue key, const char * name)
    {
        if ( ! isSymbol(key) ) {
            return badSymbol(key, name);
        }

        if ( ! truth(env->check(key, false)) ) {
            return missingSymbol(key, name);
        }

        return makeEnv(env);
    }

    string findImportLib(ScamValue lib)
    {
        string fileToLoad { "" };
        stringstream s;

        s << lib->stringValue() << ".scm";
        fileToLoad = findFileOnPath(s.str());
        if ( ! fileToLoad.empty() ) {
            return fileToLoad;
        }

        s.str("");
        s << lib->stringValue() << ".def";
        return findFileOnPath(s.str());
    }

    ScamValue unknownImportDirective(ScamValue arg)
    {
        ScamValue err = makeError("Unknown import directive: %{0}", arg);
        err->errorCategory() = importCategory;
        return err;
    }

    ScamValue libraryNotFound(ScamValue symbol)
    {
        ScamValue err = makeError("Library not found: %{0}", symbol);
        err->errorCategory() = importCategory;
        return err;
    }

    ScamValue importError(ScamValue lib, ScamValue error)
    {
        ScamValue err =
            makeError("Error during import of library %{0}: %{1}", lib, error);
        err->errorCategory() = importCategory;
        return err;
    }

    ScamValue badPrefix(ScamValue args)
    {
        ScamValue err = makeError("Cannot process prefix, got %{0}", args);
        err->errorCategory() = importCategory;
        return err;
    }

    ScamValue insufficientParameters(ScamValue args, const char * where)
    {
        ScamValue err =
            makeError("Insufficient parameters for %{0}: %{1}",
                      makeString(where),
                      args);
        err->errorCategory() = importCategory;
        return err;
    }

    ScamValue badSymbol(ScamValue arg, const char * where)
    {
        ScamValue err =
            makeError("Directive %{0} expects symbol name: %{1}",
                      makeString(where),
                      arg);
        err->errorCategory() = importCategory;
        return err;
    }

    ScamValue missingSymbol(ScamValue arg, const char * where)
    {
        ScamValue err =
            makeError("Directive %{0} does not have symbol available: %{1}",
                      makeString(where),
                      arg);
        err->errorCategory() = importCategory;
        return err;
    }
}
