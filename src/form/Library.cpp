#include "form/AllSpecialForms.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/SequenceOps.hpp"
#include "util/FileUtils.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern Env * newEnv();
    extern Env * extendEnv(Env * env);

    extern ScamValue getLibraryName(ScamValue & args);
    extern ScamValue getDirectiveName(ScamValue arg0);

    extern ScamValue importImportSet(ScamValue arg);

    extern ScamValue importLib(ScamValue symbol);
    extern ScamValue importOnly(ScamValue args);
    extern ScamValue importExcept(ScamValue args);
    extern ScamValue importPrefix(ScamValue args);
    extern ScamValue importRename(ScamValue args);
    extern ScamValue loadLibraryFromFile(ScamValue arg);
    extern ScamValue lookForLibrary(ScamValue arg, ScamValue name);

    extern ScamValue importCommon(ScamValue args, const char * name);

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

void scam::applyDefineLibrary(ScamValue args, Continuation * cont, Env * env)
{
    ScamValue result = defineLibrary(args);
    if ( isUnhandledError(result) ) {
        ScamEngine::getEngine().handleError(result);
    }
}

void scam::applyImport(ScamValue args, Continuation * cont, Env * env)
{
    ScamValue result = importToEnv(args);
    if ( isUnhandledError(result) ) {
        ScamEngine::getEngine().handleError(result);
    }
    else if ( isEnv(result) ) {
        Env * dst = ScamEngine::getEngine().getFrame();
        Env * src = asEnv(result);
        dst->merge(src);
        cont->handleValue(makeNothing());
    }
}

ScamValue scam::defineLibrary(ScamValue args)
{
    ScamEngine & engine = ScamEngine::getEngine();
    MemoryManager & mm = engine.getMemoryManager();
    bool suppressed = mm.isSuppressed();
    mm.setSuppressed(true);

    ScamValue name = getLibraryName(args);
    if ( isUnhandledError(name) ) {
        return name;
    }

    vector<ScamValue> imports;
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
        else if ( equals(result, makeSymbol("import")) ) {
            imports.push_back(arg0);
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

    Env * original = engine.getFrame();
    Env * extended = extendEnv(original);
    engine.setFrame(extended);

    for ( const auto i : imports ) {
        ScamValue specs = getCdr(i);
        while ( ! isNull(specs) ) {
            ScamValue spec = getCar(specs);
            specs = getCdr(specs);
            ScamValue result = importImportSet(spec);
            if ( isEnv(result) ) {
                Env * dst = extended;
                Env * src = asEnv(result);
                dst->merge(src);
            }
            else {
                return result;
            }
        }
    }

    for ( const auto d : defines ) {
        ScamValue result = engine.eval(d);
        if ( isUnhandledError(result) ) {
            return result;
        }
    }

    engine.setFrame(original);

    Env * lib = newEnv();
    if ( exports.empty() ) {
        Env * dst = lib;
        Env * src = extended;
        dst->merge(src);
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
    engine.saveLibrary(name, lib);
    mm.setSuppressed(suppressed);
    return rv;
}

ScamValue scam::importToEnv(ScamValue args, Env * target)
{
    ScamEngine & engine = ScamEngine::getEngine();
    MemoryManager & mm = engine.getMemoryManager();
    bool suppressed = mm.isSuppressed();
    mm.setSuppressed(true);

    Env * original = engine.getFrame();
    Env * local    = extendEnv(original);
    engine.setFrame(local);
    if ( nullptr == target ) {
        target = newEnv();
    }
    ScamValue rv = makeEnv(target);

    while ( ! isNull(args) ) {
        ScamValue arg0 = getCar(args);
        args = getCdr(args);

        ScamValue result = importImportSet(arg0);
        if ( isEnv(result) ) {
            Env * dst = target;
            Env * src = asEnv(result);
            dst->merge(src);
        }
        else {
            rv = result;
            break;
        }
    }

    engine.setFrame(original);
    mm.setSuppressed(suppressed);
    return rv;
}

namespace
{
    Env * newEnv()
    {
        Env * e = ScamEngine::getEngine().getMemoryManager().make<Env>();
        return e;
    }

    Env * extendEnv(Env * env)
    {
        Env * e = env->extend();
        return e;
    }

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
             equals(type, makeSymbol("import")) ||
             equals(type, makeSymbol("export")) ) {
            return type;
        }

        return unknownLibraryDirective(type);
    }

    ScamValue importImportSet(ScamValue arg)
    {
        ScamValue rv = makeNothing();

        if ( isSymbol(arg) ) {
            rv = importLib(arg);
        }

        else if ( isPair(arg) ) {
            ScamValue directive = getCar(arg);
            ScamValue rest      = getCdr(arg);

            if ( isSymbol(directive) ) {
                const string & name = directive->stringValue();
                if ( name == "only" ) {
                    rv = importOnly(rest);
                }

                else if ( name == "except" ) {
                    rv = importExcept(rest);
                }

                else if ( name == "prefix" ) {
                    rv = importPrefix(rest);
                }

                else if ( name == "rename" ) {
                    rv = importRename(rest);
                }

                else {
                    rv = loadLibraryFromFile(arg);
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

    ScamValue importLib(ScamValue symbol)
    {
        string fileToLoad = findImportLib(symbol);
        if ( fileToLoad.empty() ) {
            return libraryNotFound(symbol);
        }

        ScamEngine & engine = ScamEngine::getEngine();
        Env * original = engine.getFrame();
        Env * extended = extendEnv(original);
        engine.setFrame(extended);
        ScamValue rv = loadEvalFile(fileToLoad);
        engine.setFrame(original);

        if ( isError(rv) ) {
            return importError(symbol, rv);
        }

        rv = makeEnv(extended);
        return rv;
    }

    ScamValue importOnly(ScamValue args)
    {
        ScamValue result = importCommon(args, "only");
        if ( ! isEnv(result) ) {
            return result;
        }

        Env * temp = asEnv(result);
        Env * base = newEnv();
        ScamValue rest = getCdr(args);

        return copyOnlyRename(base, temp, rest, "only");
    }

    ScamValue importExcept(ScamValue args)
    {
        ScamValue result = importCommon(args, "except");
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

    ScamValue importPrefix(ScamValue args)
    {
        ScamValue rest = getCdr(args);
        ScamValue p    = getCar(rest);
        if ( ( ! isSymbol(p)) || ( ! isNull(getCdr(rest))) ) {
            return badPrefix(args);
        }
        const string & prefix = p->stringValue();

        ScamValue result = importCommon(args, "prefix");
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

    ScamValue importRename(ScamValue args)
    {
        ScamValue result = importCommon(args, "rename");
        if ( ! isEnv(result) ) {
            return result;
        }

        Env * temp = asEnv(result);
        ScamValue rest = getCdr(args);

        return copyOnlyRename(temp, temp, rest, "rename");
    }

    ScamValue loadLibraryFromFile(ScamValue arg)
    {
        ScamValue lib = ScamEngine::getEngine().findLibrary(arg);
        if ( isEnv(lib) ) {
            return lib;
        }

        lib = lookForLibrary(arg, arg);
        if ( isEnv(lib) ) {
            return lib;
        }

        ScamValue arg2 = makePair(makeSymbol("lib"), arg);
        return lookForLibrary(arg2, arg);
    }

    ScamValue lookForLibrary(ScamValue arg, ScamValue name)
    {
        stringstream s;
        while ( ! isNull(arg) ) {
            ScamValue element = getCar(arg);
            arg = getCdr(arg);
            s << element->stringValue();
            if ( ! (isNull(arg)) ) {
                s << "/";
            }
        }

        ScamValue text = makeSymbol(s.str());

        const string path = findImportLib(text);
        if ( ! path.empty() ) {
            (void) loadEvalFile(path);
            ScamValue lib = ScamEngine::getEngine().findLibrary(name);
            if ( isEnv(lib) ) {
                return lib;
            }
        }

        ScamValue error = unknownImportDirective(name);
        return error;
    }

    ScamValue importCommon(ScamValue args, const char * name)
    {
        ScamValue rv = makeNothing();

        if ( ! isPair(args) ) {
            rv = insufficientParameters(args, name);
        }
        else {
            ScamValue arg = getCar(args);
            rv = importImportSet(arg);
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
        s << lib->stringValue() << ".lib";
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
