#include "form/AllSpecialForms.hpp"

#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
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
    extern Env * importImportSet(ScamValue arg, ScamEngine * engine);

    extern Env * importLib(ScamValue symbol, ScamEngine * engine);
    extern Env * importOnly(ScamValue args, ScamEngine * engine);
    extern Env * importExcept(ScamValue args, ScamEngine * engine);
    extern Env * importPrefix(ScamValue args, ScamEngine * engine);
    extern Env * importRename(ScamValue args, ScamEngine * engine);

    extern Env *
    importCommon(ScamValue args, ScamEngine * engine, const char * name);

    extern Env * validateKey(Env * env,
                             ScamValue key,
                             ScamEngine * engine,
                             const char * name);

    extern string findImportLib(ScamValue lib);

    extern void libraryNotFound(ScamValue symbol, ScamEngine * engine);
    extern void unknownImportDirective(ScamValue arg, ScamEngine * engine);

    extern void
    importError(ScamValue lib, ScamValue error, ScamEngine * engine);

    extern void badPrefix(ScamValue args, ScamEngine * engine);

    extern void insufficientParameters(ScamValue args,
                                       ScamEngine * engine,
                                       const char * where);

    extern void
    badSymbol(ScamValue arg, ScamEngine * engine, const char * where);

    extern void
    missingSymbol(ScamValue arg, ScamEngine * engine, const char * where);

    extern void finishError(ScamValue err, ScamEngine * engine);
}

void scam::applyImport(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    engine->pushFrame();

    while ( ! isNull(args) ) {
        ScamValue arg0 = getCar(args);
        args = getCdr(args);

        Env * env = importImportSet(arg0, engine);
        if ( env ) {
            engine->getFrame()->merge(env);
        }
        else {
            engine->popFrame();
            break;
        }
    }
}

namespace
{
    Env * importImportSet(ScamValue arg, ScamEngine * engine)
    {
        Env * temp;
        engine->pushFrame();

        if ( isSymbol(arg) ) {
            temp = importLib(arg, engine);
        }

        else if ( isPair(arg) ) {
            ScamValue directive = getCar(arg);
            ScamValue rest      = getCdr(arg);

            if ( isSymbol(directive) ) {
                const string & name = directive->stringValue();
                if ( name == "only" ) {
                    temp = importOnly(rest, engine);
                }

                else if ( name == "except" ) {
                    temp = importExcept(rest, engine);
                }

                else if ( name == "prefix" ) {
                    temp = importPrefix(rest, engine);
                }

                else if ( name == "rename" ) {
                    temp = importRename(rest, engine);
                }

                else {
                    unknownImportDirective(arg, engine);
                    return nullptr;
                }
            }
            else {
                unknownImportDirective(arg, engine);
                temp = nullptr;
            }
        }

        else {
            unknownImportDirective(arg, engine);
            temp = nullptr;
        }

        engine->popFrame();
        if ( temp ) {
            Env * env = engine->getFrame();
            env->merge(temp);
        }

        return temp;
    }

    Env * importLib(ScamValue symbol, ScamEngine * engine)
    {
        string fileToLoad = findImportLib(symbol);
        if ( fileToLoad.empty() ) {
            libraryNotFound(symbol, engine);
            return nullptr;
        }

        ScamValue rv = loadEvalFile(fileToLoad, engine);
        if ( isUnhandledError(rv) ) {
            importError(symbol, rv, engine);
            return nullptr;
        }

        return engine->getFrame();
    }

    Env * importOnly(ScamValue args, ScamEngine * engine)
    {
        Env * temp = importCommon(args, engine, "only");
        if ( ! temp ) {
            return nullptr;
        }

        ScamValue rest = getCdr(args);
        Env * base = engine->getFrame();
        while ( isPair(rest) ) {
            ScamValue arg0 = getCar(rest);
            rest           = getCdr(rest);

            if ( ! validateKey(temp, arg0, engine, "only") ) {
                return nullptr;
            }

            base->put(arg0, temp->get(arg0));
        }

        return base;
    }

    Env * importExcept(ScamValue args, ScamEngine * engine)
    {
        Env * temp = importCommon(args, engine, "except");
        if ( ! temp ) {
            return nullptr;
        }

        ScamValue rest = getCdr(args);
        Env * base = engine->getFrame();
        while ( isPair(rest) ) {
            ScamValue arg0 = getCar(rest);
            rest           = getCdr(rest);

            if ( ! validateKey(temp, arg0, engine, "except") ) {
                return nullptr;
            }

            temp->remove(arg0);
        }

        ScamValue rv = base->merge(temp);
        if ( isUnhandledError(rv) ) {
            engine->popFrame();
            engine->handleError(rv);
            return nullptr;
        }

        return base;
    }

    Env * importPrefix(ScamValue args, ScamEngine * engine)
    {
        ScamValue rest = getCdr(args);
        ScamValue p    = getCar(rest);
        if ( ( ! isSymbol(p)) || ( ! isNull(getCdr(rest))) ) {
            badPrefix(args, engine);
            return nullptr;
        }
        const string & prefix = p->stringValue();

        Env * temp = importCommon(args, engine, "prefix");
        if ( ! temp ) {
            return nullptr;
        }

        Env * base = engine->getFrame();
        set<string> keys;
        temp->getKeys(keys);

        for ( const auto k : keys ) {
            stringstream s;
            s << prefix << k;
            ScamValue key = makeSymbol(s.str());
            base->put(key, temp->get(makeSymbol(k)));
        }

        return base;
    }

    Env * importRename(ScamValue args, ScamEngine * engine)
    {
        Env * temp = importCommon(args, engine, "rename");
        if ( ! temp ) {
            return nullptr;
        }

        ScamValue rest = getCdr(args);
        Env * base = engine->getFrame();
        while ( isPair(rest) ) {
            ScamValue arg0 = getCar(rest);
            rest           = getCdr(rest);

            SymbolParameter p0;
            SymbolParameter p1;
            ScamValue test = argsToParmsMsg(arg0, p0, p1);
            if ( isUnhandledError(test)  ) {
                engine->handleError(test);
                engine->popFrame();
                return nullptr;
            }

            ScamValue oldName = p0.value;
            if ( ! validateKey(temp, oldName, engine, "rename") ) {
                return nullptr;
            }

            ScamValue newName = p1.value;
            ScamValue value   = temp->get(oldName);
            temp->remove(oldName);
            temp->put(newName, value);
        }

        base->merge(temp);
        return base;
    }

    Env * importCommon(ScamValue args, ScamEngine * engine, const char * name)
    {
        engine->pushFrame();
        Env * temp;

        if ( ! isPair(args) ) {
            insufficientParameters(args, engine, name);
            temp = nullptr;
        }

        ScamValue arg = getCar(args);
        temp = importImportSet(arg, engine);
        engine->popFrame();
        return temp;
    }

    Env * validateKey(Env * env,
                      ScamValue key,
                      ScamEngine * engine,
                      const char * name)
    {
        if ( ! isSymbol(key) ) {
            badSymbol(key, engine, name);
            return nullptr;
        }

        if ( ! truth(env->check(key, false)) ) {
            missingSymbol(key, engine, name);
            return nullptr;
        }

        return env;
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

    void unknownImportDirective(ScamValue arg, ScamEngine * engine)
    {
        ScamValue err = makeError("Unknown import directive: %{0}", arg);
        finishError(err, engine);
    }

    void libraryNotFound(ScamValue symbol, ScamEngine * engine)
    {
        ScamValue err = makeError("Library not found: %{0}", symbol);
        finishError(err, engine);
    }

    void importError(ScamValue lib, ScamValue error, ScamEngine * engine)
    {
        ScamValue err =
            makeError("Error during import of library %{0}: %{1}", lib, error);
        finishError(err, engine);
    }

    void badPrefix(ScamValue args, ScamEngine * engine)
    {
        ScamValue err = makeError("Cannot process prefix, got %{0}", args);
        finishError(err, engine);
    }


    void insufficientParameters(ScamValue args,
                                ScamEngine * engine,
                                const char * where)
    {
        ScamValue err =
            makeError("Insufficient parameters for %{0}: %{1}",
                      makeString(where),
                      args);
        finishError(err, engine);
    }

    void badSymbol(ScamValue arg, ScamEngine * engine, const char * where)
    {
        ScamValue err =
            makeError("Directive %{0} expects symbol name: %{1}",
                      makeString(where),
                      arg);
        finishError(err, engine);
    }

    void missingSymbol(ScamValue arg, ScamEngine * engine, const char * where)
    {
        ScamValue err =
            makeError("Directive %{0} does not have symbol available: %{1}",
                      makeString(where),
                      arg);
        finishError(err, engine);
    }

    void finishError(ScamValue err, ScamEngine * engine)
    {
        err->errorCategory() = importCategory;
        engine->popFrame();
        engine->handleError(err);
    }
}
