#include "env/EnvOps.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "form/AllSpecialForms.hpp"
#include "port/CerrPort.hpp"
#include "port/CinPort.hpp"
#include "port/CoutPort.hpp"
#include "prim/AllPrimitives.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern void mergeLibrary(Env * env, ScamValue key);

    extern Env * initializeSchemeBase(Env * base);
    extern Env * initializeSchemeChar(Env * base);
    extern Env * initializeSchemeCxr(Env * base);
    extern Env * initializeSchemeEval(Env * base);
    extern Env * initializeSchemeInexact(Env * base);
    extern Env * initializeSchemeLoad(Env * base);
    extern Env * initializeSchemeRead(Env * base);
    extern Env * initializeSchemeRepl(Env * base);
    extern Env * initializeSchemeWrite(Env * base);
    extern Env * initializeScamBase(Env * base);
    extern Env * initializeScamBacktrack(Env * base);
    extern Env * initializeScamClass(Env * base);
    extern Env * initializeScamError(Env * base);
    extern Env * initializeScamUnify(Env * base);
    extern Env * initializeScamMisc(Env * base);

    extern void addSpecialForm(Env * env, const char * name, SfFunction func);
    extern void addPrimitive(Env * env, const char * name, PrimFunction func);
    extern void addPorts(Env * env);

    extern void doPut(Env * env, ScamValue key, ScamValue value);

    extern void safeInclude(Env * env, const char * filename);
}

Env * scam::getConfigurationEnv()
{
    Env * env = ScamEngine::getEngine().getMemoryManager().make<Env>();
    addPorts(env);
    return env;
}

Env * scam::getSyntaxEnv(Env * base)
{
    Env * env = base->extend();
    addSpecialForm(env, "and", applyAnd);
    addSpecialForm(env, "case", applyCase);
    addSpecialForm(env, "cond", applyCond);
    addSpecialForm(env, "define", applyDefine);
    addSpecialForm(env, "define-library", applyDefineLibrary);
    addSpecialForm(env, "define-syntax", applyDefineSyntax);
    addSpecialForm(env, "if", applyIf);
    addSpecialForm(env, "import", applyImport);
    addSpecialForm(env, "lambda", applyLambda);
    addSpecialForm(env, "let", applyLet);
    addSpecialForm(env, "let*", applyLetStar);
    addSpecialForm(env, "letrec", applyLetRec);
    addSpecialForm(env, "or", applyOr);
    addSpecialForm(env, "quasiquote", applyQuasiQuote);
    addSpecialForm(env, "quote", applyQuote);
    addSpecialForm(env, "set!", applySetX);

    addPrimitive(env, "begin", applyBegin);
    addPrimitive(env, "include", applyInclude);

    safeInclude(env, "lib/scheme/syntax.lib");

    return env;
}

void scam::initalizeLibraries(Env * base)
{
    initializeSchemeBase(base);
    initializeSchemeChar(base);
    initializeSchemeCxr(base);
    initializeSchemeEval(base);
    initializeSchemeInexact(base);
    initializeSchemeLoad(base);
    initializeSchemeRead(base);
    initializeSchemeRepl(base);
    initializeSchemeWrite(base);
    initializeScamBase(base);
    initializeScamBacktrack(base);
    initializeScamClass(base);
    initializeScamError(base);
    initializeScamUnify(base);
    initializeScamMisc(base);
}

Env * scam::makeInteractionEnv(Env * base)
{
    Env * env = base->extend();
    ScamValue scheme = makeSymbol("scheme");
    mergeLibrary(env, makeList(scheme, makeSymbol("base")));
    ScamValue scam = makeSymbol("scam");
    mergeLibrary(env, makeList(scam, makeSymbol("base")));
    return env;
}

void scam::applyInteractionEnv(ScamValue args, Continuation * cont)
{
    static const char * name = "interaction-environment";

    if ( argsToParms(args, name) ) {
        Env * e = ScamEngine::getEngine().getInteractionFrame();
        ScamValue iEnv = makeEnv(e);
        cont->handleValue(iEnv);
    }
}

void scam::applyEnvironment(ScamValue args, Continuation * cont)
{
    static const char * name = "environment";

    ObjectParameter  pObj;
    CountedParameter p0(pObj, 1);
    if ( argsToParms(args, name, p0) ) {
        ScamEngine & engine = ScamEngine::getEngine();
        Env * env = engine.getSyntaxFrame()->extend();
        ScamValue rv = importToEnv(p0.value, env);
        if ( isUnhandledError(rv) ) {
            engine.handleError(rv);
        }
        else {
            cont->handleValue(rv);
        }
    }
}

void scam::applyEval(ScamValue args, Continuation * cont)
{
    static const char * name = "eval";

    ObjectParameter p0;
    EnvParameter    p1;
    if ( argsToParms(args, name, p0, p1) ) {
        Env * env = asEnv(p1.value);
        eval(p0.value, cont, env);
    }
}

namespace
{
    void mergeLibrary(Env * env, ScamValue key)
    {
        ScamEngine & engine = ScamEngine::getEngine();
        ScamValue lib = engine.findLibrary(key);
        if ( isEnv(lib) ) {
            env->merge(asEnv(lib));
        }
    }

    Env * initializeSchemeBase(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "%", applyMod);
        addPrimitive(env, "*", applyMul);
        addPrimitive(env, "+", applyAdd);
        addPrimitive(env, "-", applySub);
        addPrimitive(env, "/", applyDiv);

        addPrimitive(env, "<>", applyNe);
        addPrimitive(env, "=",  applyEq);
        addPrimitive(env, "<",  applyLt);
        addPrimitive(env, "<=", applyLe);
        addPrimitive(env, ">",  applyGt);
        addPrimitive(env, ">=", applyGe);

        addSpecialForm(env, "apply", applyApply);
        addPrimitive(env, "append", applyAppend);

        addPrimitive(env, "boolean?", applyBoolP);

        addSpecialForm(env, "call/cc", applyCallCC);
        addPrimitive(env, "car", applyCar);
        addPrimitive(env, "cdr", applyCdr);
        addPrimitive(env, "char?", applyCharP);
        addPrimitive(env, "complex?", applyComplexP);
        addPrimitive(env, "cons", applyCons);

        addPrimitive(env, "eof-object", applyEofObject);
        addPrimitive(env, "eof-object?", applyEofP);
        addPrimitive(env, "eq?", applyEqP);
        addPrimitive(env, "equal?", applyEqualP);
        addPrimitive(env, "eqv?", applyEqvP);
        addPrimitive(env, "error", applyError);
        addPrimitive(env, "error-object-irritants", applyErrorIrritant);
        addPrimitive(env, "error-object-message", applyErrorMessage);
        addPrimitive(env, "error-object?", applyErrorP);
        addPrimitive(env, "exact?", applyExactP);

        addPrimitive(env, "file-error?", applyFileErrorP);

        addPrimitive(env, "get-output-string", applyGetOutStr);

        addPrimitive(env, "inexact?", applyInexactP);
        addPrimitive(env, "integer?", applyIntegerP);

        addPrimitive(env, "list", applyList);
        addPrimitive(env, "list->string", applyList2String);
        addPrimitive(env, "list?", applyListP);

        addPrimitive(env, "make-list", applyMakeList);
        addPrimitive(env, "make-string", applyMakeString);

        addPrimitive(env, "newline", applyNewline);
        addSpecialForm(env, "not", applyNot);
        addPrimitive(env, "null?", applyNullP);

        addPrimitive(env, "open-input-string", applyOpenInStr);
        addPrimitive(env, "open-output-string", applyOpenOutStr);

        addPrimitive(env, "pair?", applyPairP);
        addPrimitive(env, "port?", applyPortP);
        addPrimitive(env, "procedure?", applyProcedureP);

        addPrimitive(env, "raise", applyRaise);
        addPrimitive(env, "rational?", applyRationalP);
        addPrimitive(env, "read-error?", applyReadErrorP);
        addPrimitive(env, "real?", applyRealP);

        addPrimitive(env, "set-car!", applySetCarX);
        addPrimitive(env, "set-cdr!", applySetCdrX);
        addPrimitive(env, "string", applyString);
        addPrimitive(env, "string->list", applyString2List);
        addPrimitive(env, "string-append", applyStringAppend);
        addPrimitive(env, "string-copy!", applyStringCopyX);
        addPrimitive(env, "string-copy", applyStringCopy);
        addPrimitive(env, "string-fill!", applyStringFillX);
        addPrimitive(env, "string-length", applyStringLength);
        addPrimitive(env, "string-ref", applyStringRef);
        addPrimitive(env, "string-set!", applyStringSetX);
        addPrimitive(env, "string<=?", applyStringLeP);
        addPrimitive(env, "string<?", applyStringLtP);
        addPrimitive(env, "string=?", applyStringEqP);
        addPrimitive(env, "string>=?", applyStringGeP);
        addPrimitive(env, "string>?", applyStringGtP);
        addPrimitive(env, "string?", applyStringP);
        addPrimitive(env, "symbol?", applySymbolP);

        addPrimitive(env, "vector?", applyVectorP);

        addPrimitive(env, "with-exception-handler", applyWithHandler);

        safeInclude(env, "lib/scheme/base.lib");

        ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("base"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeChar(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "string-ci<=?", applyStringCiLeP);
        addPrimitive(env, "string-ci<?", applyStringCiLtP);
        addPrimitive(env, "string-ci=?", applyStringCiEqP);
        addPrimitive(env, "string-ci>=?", applyStringCiGeP);
        addPrimitive(env, "string-ci>?", applyStringCiGtP);
        addPrimitive(env, "string-downcase", applyStringDowncase);
        addPrimitive(env, "string-upcase", applyStringUpcase);

        safeInclude(env, "lib/scheme/char.lib");

        ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("char"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeCxr(Env * base)
    {
        Env * env = base->extend();

        safeInclude(env, "lib/scheme/cxr.lib");

        ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("cxr"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeEval(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "environment", applyEnvironment);
        addPrimitive(env, "eval", applyEval);

        ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("eval"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeInexact(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "finite?", applyFiniteP);
        addPrimitive(env, "infinite?", applyInfiniteP);
        addPrimitive(env, "nan?", applyNanP);

        ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("inexact"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeLoad(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "load", applyLoad);

        ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("load"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeRead(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "read", applyRead);

        ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("read"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeRepl(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "interaction-environment", applyInteractionEnv);

        ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("repl"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeWrite(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "display", applyDisplay);

        ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("write"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeScamBase(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "dict?", applyDictP);
        addPrimitive(env, "keyword?", applyKeywordP);
        addPrimitive(env, "numeric?", applyNumericP);
        addPrimitive(env, "vlen", applyVLen);
        addPrimitive(env, "vref", applyVRef);

        safeInclude(env, "lib/scam/base.lib");

        ScamValue name = makeList(makeSymbol("scam"), makeSymbol("base"));
        ScamEngine::getEngine().saveLibrary(name, env);

        return env;
    }

    Env * initializeScamBacktrack(Env * base)
    {
        Env * env = base->extend();

        addSpecialForm(env, "amb", applyAmb);
        addPrimitive(env, "backtrack", applyBacktrack);

        safeInclude(env, "lib/scam/backtrack.lib");

        ScamValue name = makeList(makeSymbol("scam"), makeSymbol("backtrack"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeScamClass(Env * base)
    {
        Env * env = base->extend();

        addSpecialForm(env, "make-class", applyClassMaker);
        addPrimitive(env, "class?", applyClassP);
        addPrimitive(env, "instance?", applyInstanceP);

        ScamValue name = makeList(makeSymbol("scam"), makeSymbol("class"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeScamError(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "make-error", applyMakeError);
        addPrimitive(env, "error-category", applyErrorCat);
        addPrimitive(env, "error->string", applyError2String);

        ScamValue name = makeList(makeSymbol("scam"), makeSymbol("error"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeScamUnify(Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "match", applyMatch);
        addPrimitive(env, "unify", applyUnify);
        addPrimitive(env, "substitute", applySubstitute);
        addPrimitive(env, "instantiate", applyInstantiate);

        ScamValue name = makeList(makeSymbol("scam"), makeSymbol("unify"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    Env * initializeScamMisc(Env * base)
    {
        Env * env = base->extend();

        addSpecialForm(env, "syntax-expand", applySyntaxExpand);
        addSpecialForm(env, "undefine", applyUndefine);
        addPrimitive(env, "environment?",  applyEnvironmentP);
        addPrimitive(env, "spawn", applySpawn);

        safeInclude(env, "lib/scam/misc.lib");

        ScamValue name = makeList(makeSymbol("scam"), makeSymbol("misc"));
        ScamEngine::getEngine().saveLibrary(name, env);
        return env;
    }

    void addSpecialForm(Env * env, const char * name, SfFunction func)
    {
        ScamValue key  = makeSymbol(name);
        ScamValue form = makeSpecialForm(name, func, true);
        doPut(env, key, form);
    }

    void addPrimitive(Env * env, const char * name, PrimFunction func)
    {
        ScamValue key  = makeSymbol(name);
        ScamValue form = makePrimitive(name, func, true);
        doPut(env, key, form);
    }

    void addPorts(Env * env)
    {
        ScamPort * cinPort  = new CinPort;
        ScamPort * coutPort = new CoutPort;
        ScamPort * cerrPort = new CerrPort;

        doPut(env, makeSymbol("**cin**"),  makePort(cinPort));
        doPut(env, makeSymbol("**cout**"), makePort(coutPort));
        doPut(env, makeSymbol("**cerr**"), makePort(cerrPort));
    }

    void doPut(Env * env, ScamValue key, ScamValue value)
    {
        ScamValue test = env->put(key, value);
        if ( isError(test) ) {
            throw ScamException(writeValue(test));
        }
    }

    void safeInclude(Env * env, const char * filename)
    {
        ScamEngine & engine = ScamEngine::getEngine();
        Env * old = engine.getFrame();
        engine.setFrame(env);
        (void) loadHelper(filename);
        engine.setFrame(old);
    }
}
