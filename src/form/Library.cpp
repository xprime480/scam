#include "form/AllSpecialForms.hpp"

#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/FileUtils.hpp"
#include "util/MemoryManager.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern ScamValue getLibraryName(ScamValue & args);
    extern ScamValue getDirectiveName(ScamValue arg0);

    extern ScamValue importImportSet(ScamValue arg, ScamEngine * engine);

    extern ScamValue importLib(ScamValue symbol, ScamEngine * engine);
    extern ScamValue importOnly(ScamValue args, ScamEngine * engine);
    extern ScamValue importExcept(ScamValue args, ScamEngine * engine);
    extern ScamValue importPrefix(ScamValue args, ScamEngine * engine);
    extern ScamValue importRename(ScamValue args, ScamEngine * engine);
    extern ScamValue loadLibraryFromFile(ScamValue arg, ScamEngine * engine);

    extern ScamValue
    importCommon(ScamValue args, ScamEngine * engine, const char * name);

    extern ScamValue validateKey(Env * env, ScamValue key, const char * name);
    extern string findImportLib(ScamValue lib);

    ScamValue
    copyOnlyRename(Env * dst, Env * src, ScamValue symbols, const char * name);

    /*** error messages ***/

    extern ScamValue missingLibraryName();
    extern ScamValue unknownLibraryDirective(ScamValue arg);

    extern ScamValue libraryNotFound(ScamValue symbol);
    extern ScamValue unknownImportDirective(ScamValue arg);
    extern ScamValue importError(ScamValue lib, ScamValue error);
    extern ScamValue badPrefix(ScamValue args);
    extern ScamValue insufficientParameters(ScamValue args, const char * where);
    extern ScamValue badSymbol(ScamValue arg, const char * where);
    extern ScamValue missingSymbol(ScamValue arg, const char * where);
}

void scam::applyDefineLibrary(ScamValue args,
                              Continuation * cont,
                              Env * env,
                              ScamEngine * engine)
{
    ScamValue result = defineLibrary(args, engine);
    if ( isUnhandledError(result) ) {
        engine->handleError(result);
    }
}

void scam::applyImport(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    ScamValue result = importToEnv(args, engine);
    if ( isUnhandledError(result) ) {
        engine->handleError(result);
    }
    else if ( isEnv(result) ) {
        engine->getFrame()->merge(asEnv(result));
    }
}

ScamValue scam::defineLibrary(ScamValue args, ScamEngine * engine)
{
    ScamValue name = getLibraryName(args);
    if ( isUnhandledError(name) ) {
        return name;
    }

    vector<ScamValue> exports;
    vector<ScamValue> defines;

    while ( ! isNull(args) ) {
        ScamValue arg0 = getCar(args);
        args           = getCdr(args);

        ScamValue result = getDirectiveName(arg0);
        if ( isUnhandledError(result) ) {
            return result;
        }

        if ( equals(result, makeSymbol("begin")) ) {
            defines.push_back(arg0);
        }
        else if ( equals(result, makeSymbol("export")) ) {
            exports.push_back(arg0);
        }
        else {
            ScamValue err =
                makeError("Internal Error: unknown directive", arg0);
            err->errorCategory() = argsCategory;
            return err;
        }
    }

    Env * original = engine->getFrame();
    Env * extended = original->extend();
    engine->setFrame(extended);

    for ( const auto d : defines ) {
        ScamValue result = engine->eval(d);
        if ( isUnhandledError(result) ) {
            return result;
        }
    }

    engine->setFrame(original);

    Env * lib = standardMemoryManager.make<Env>();
    if ( exports.empty() ) {
        lib->merge(extended);
    }
    else {
        for ( auto e : exports ) {
            ScamValue symbols = getCdr(e);
            ScamValue result =
                copyOnlyRename(lib, extended, symbols, "define-library");
            if ( isUnhandledError(result)  ) {
                return result;
            }
        }
    }

    ScamValue rv = makeEnv(lib);
    engine->saveLibrary(name, lib);
    return rv;
}

ScamValue scam::importToEnv(ScamValue args, ScamEngine * engine)
{
    Env * original = engine->getFrame();
    engine->setFrame(original->extend());
    Env * target = standardMemoryManager.make<Env>();
    ScamValue rv = makeEnv(target);

    while ( ! isNull(args) ) {
        ScamValue arg0 = getCar(args);
        args = getCdr(args);

        ScamValue result = importImportSet(arg0, engine);
        if ( isEnv(result) ) {
            target->merge(asEnv(result));
        }
        else {
            rv = result;
            break;
        }
    }

    engine->setFrame(original);
    return rv;
}

namespace
{
    ScamValue getLibraryName(ScamValue & args)
    {
        if ( isNull(args) ) {
            return missingLibraryName();
        }

        ScamValue nameArg = getCar(args);
        args              = getCdr(args);

        SymbolParameter  symParm;
        CountedParameter p0(symParm, 1);
        CountParameter   countParm;
        CountedParameter p1(countParm);

        ScamValue rv = argsToParmsMsg(nameArg, p0, p1);
        if ( ! isUnhandledError(rv) ) {
            rv   = p0.value;
        }

        return rv;
    }

    ScamValue getDirectiveName(ScamValue arg)
    {
        if ( ! isList(arg) ) {
            return unknownLibraryDirective(arg);
        }

        ScamValue type = getCar(arg);
        if ( equals(type, makeSymbol("begin")) ||
             equals(type, makeSymbol("export")) ) {
            return type;
        }

        return unknownLibraryDirective(type);
    }

    ScamValue importImportSet(ScamValue arg, ScamEngine * engine)
    {
        ScamValue rv = makeNothing();

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
                    rv = loadLibraryFromFile(arg, engine);
                }
            }
            else {
                rv = unknownImportDirective(arg);
            }
        }

        else {
            rv = unknownImportDirective(arg);
        }

        return rv;
    }

    ScamValue importLib(ScamValue symbol, ScamEngine * engine)
    {
        string fileToLoad = findImportLib(symbol);
        if ( fileToLoad.empty() ) {
            return libraryNotFound(symbol);
        }

        Env * original = engine->getFrame();
        Env * extended = original->extend();
        engine->setFrame(extended);
        ScamValue rv = loadEvalFile(fileToLoad, engine);
        engine->setFrame(original);

        if ( isError(rv) ) {
            return importError(symbol, rv);
        }

        rv = makeEnv(extended);
        return rv;
    }

    ScamValue importOnly(ScamValue args, ScamEngine * engine)
    {
        ScamValue result = importCommon(args, engine, "only");
        if ( ! isEnv(result) ) {
            return result;
        }

        Env * temp = asEnv(result);
        Env * base = standardMemoryManager.make<Env>();
        ScamValue rest = getCdr(args);

        return copyOnlyRename(base, temp, rest, "only");
    }

    ScamValue importExcept(ScamValue args, ScamEngine * engine)
    {
        ScamValue result = importCommon(args, engine, "except");
        if ( ! isEnv(result) ) {
            return result;
        }

        Env * temp = asEnv(result);
        ScamValue rest = getCdr(args);

        while ( isPair(rest) ) {
            ScamValue arg0 = getCar(rest);
            rest           = getCdr(rest);

            ScamValue inner = validateKey(temp, arg0, "except");
            if ( isUnhandledError(inner)  ) {
                return inner;
            }

            temp->remove(arg0);
        }

        return result;
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

        set<string> keys;
        temp->getKeys(keys);
        for ( const auto k : keys ) {
            stringstream s;
            s << prefix << k;
            ScamValue key = makeSymbol(s.str());
            ScamValue oldKey = makeSymbol(k);
            ScamValue val = temp->get(oldKey);
            temp->remove(oldKey);
            temp->put(key, val);
        }

        return result;
    }

    ScamValue importRename(ScamValue args, ScamEngine * engine)
    {
        ScamValue result = importCommon(args, engine, "rename");
        if ( ! isEnv(result) ) {
            return result;
        }

        Env * temp = asEnv(result);
        ScamValue rest = getCdr(args);

        return copyOnlyRename(temp, temp, rest, "rename");
    }

    ScamValue loadLibraryFromFile(ScamValue arg, ScamEngine * engine)
    {
        ScamValue originalArg = arg;

        ScamValue lib = engine->findLibrary(arg);
        if ( isEnv(lib) ) {
            return lib;
        }

        stringstream s;
        while ( ! isNull(arg) ) {
            ScamValue element = getCar(arg);
            arg = getCdr(arg);
            s << element->stringValue();
            if ( ! (isNull(arg)) ) {
                s << "/";
            }
        }

        ScamValue name = makeSymbol(s.str());

        const string path = findImportLib(name);
        if ( ! path.empty() ) {
            loadEvalFile(path, engine);
            lib = engine->findLibrary(originalArg);
            if ( isEnv(lib) ) {
                return lib;
            }
        }

	ScamValue error = unknownImportDirective(originalArg);
	return error;
    }

    ScamValue
    importCommon(ScamValue args, ScamEngine * engine, const char * name)
    {
        ScamValue rv = makeNothing();

        if ( ! isPair(args) ) {
            rv = insufficientParameters(args, name);
        }
        else {
            ScamValue arg = getCar(args);
            rv = importImportSet(arg, engine);
        }

        return rv;
    }

    ScamValue validateKey(Env * env, ScamValue key, const char * name)
    {
        ScamValue rv = makePair(key, key);

        if ( ! isSymbol(key) ) {
            SymbolParameter p0;
            SymbolParameter p1;
            ScamValue test = argsToParmsMsg(key, p0, p1);
            if ( isUnhandledError(test)  ) {
                return badSymbol(key, name);
            }

            rv = makePair(p1.value, p0.value);
            key = p0.value;
        }

        if ( ! truth(env->check(key, false)) ) {
            return missingSymbol(key, name);
        }

        return rv;
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

    ScamValue
    copyOnlyRename(Env * dst, Env * src, ScamValue symbols, const char * name)
    {
        while ( isPair(symbols) ) {
            ScamValue arg0 = getCar(symbols);
            symbols        = getCdr(symbols);

            ScamValue result = validateKey(src, arg0, name);
            if ( isUnhandledError(result)  ) {
                return result;
            }

            ScamValue newName = getCar(result);
            ScamValue oldName = getCdr(result);
            ScamValue value   = src->get(oldName);
            if ( dst->check(oldName) ) {
                dst->remove(oldName);
            }
            dst->put(newName, value);
        }

        return makeEnv(dst);
    }

    ScamValue unknownImportDirective(ScamValue arg)
    {
        ScamValue err = makeError("Unknown import directive: %{0}", arg);
        err->errorCategory() = importCategory;
        return err;
    }

    ScamValue missingLibraryName()
    {
        ScamValue err = makeError("define-library: Missing library name");
        err->errorCategory() = argsCategory;
        return err;
    }

    ScamValue unknownLibraryDirective(ScamValue arg)
    {
        ScamValue err =
            makeError("define-library: Unknown directive: %{0}", arg);
        err->errorCategory() = argsCategory;
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
