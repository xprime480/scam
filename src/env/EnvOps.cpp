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
    extern void addForms(ScamEngine * engine, Env * env);

    extern Env * initializeSchemeBase(ScamEngine * engine, Env * base);
    extern Env * initializeSchemeChar(ScamEngine * engine, Env * base);
    extern Env * initializeSchemeEval(ScamEngine * engine, Env * base);
    extern Env * initializeSchemeInexact(ScamEngine * engine, Env * base);
    extern Env * initializeSchemeLoad(ScamEngine * engine, Env * base);
    extern Env * initializeSchemeRead(ScamEngine * engine, Env * base);
    extern Env * initializeSchemeRepl(ScamEngine * engine, Env * base);
    extern Env * initializeSchemeWrite(ScamEngine * engine, Env * base);
    extern Env * initializeScamBase(ScamEngine * engine, Env * base);
    extern Env * initializeScamBacktrack(ScamEngine * engine, Env * base);
    extern Env * initializeScamClass(ScamEngine * engine, Env * base);
    extern Env * initializeScamError(ScamEngine * engine, Env * base);
    extern Env * initializeScamUnify(ScamEngine * engine, Env * base);
    extern Env * initializeScamMisc(ScamEngine * engine, Env * base);

    extern void addSpecialForm(Env * env,
                               const char * name,
                               SfFunction func,
                               ScamEngine * engine);

    extern void addPrimitive(Env * env,
                             const char * name,
                             PrimFunction func,
                             ScamEngine * engine);

    extern void addPorts(Env * env, ScamEngine * engine);

    extern void doPut(Env * env, ScamValue key, ScamValue value);
}

Env * scam::getConfigurationEnv(ScamEngine * engine)
{
    Env * env = standardMemoryManager.make<Env>();
    addPorts(env, engine);
    return env;
}

Env * scam::getSyntaxEnv(ScamEngine * engine, Env * base)
{
    Env * env = base->extend();
    addSpecialForm(env, "and", applyAnd, engine);
    addSpecialForm(env, "define", applyDefine, engine);
    addSpecialForm(env, "define-library", applyDefineLibrary, engine);
    addSpecialForm(env, "define-syntax", applyDefineSyntax, engine);
    addSpecialForm(env, "if", applyIf, engine);
    addSpecialForm(env, "import", applyImport, engine);
    addSpecialForm(env, "lambda", applyLambda, engine);
    addSpecialForm(env, "let", applyLet, engine);
    addSpecialForm(env, "let*", applyLetStar, engine);
    addSpecialForm(env, "letrec", applyLetRec, engine);
    addSpecialForm(env, "or", applyOr, engine);
    addSpecialForm(env, "quasiquote", applyQuasiQuote, engine);
    addSpecialForm(env, "quote", applyQuote, engine);
    addSpecialForm(env, "set!", applySetX, engine);

    addPrimitive(env, "begin", applyBegin, engine);
    
    return env;
}

void scam::initalizeLibraries(ScamEngine * engine, Env * base)
{
    initializeSchemeBase(engine, base);
    initializeSchemeChar(engine, base);
    initializeSchemeEval(engine, base);
    initializeSchemeInexact(engine, base);
    initializeSchemeLoad(engine, base);
    initializeSchemeRead(engine, base);
    initializeSchemeRepl(engine, base);
    initializeSchemeWrite(engine, base);
    initializeScamBase(engine, base);
    initializeScamBacktrack(engine, base);
    initializeScamClass(engine, base);
    initializeScamError(engine, base);
    initializeScamUnify(engine, base);
    initializeScamMisc(engine, base);
}

Env * scam::makeInteractionEnv(ScamEngine * engine, Env * base)
{
    Env * env = base->extend();
    addForms(engine, env);
    return env;
}

void scam::applyInteractionEnv(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine)
{
    static const char * name = "interaction-environment";

    if ( argsToParms(args, engine, name) ) {
        ScamValue iEnv = makeEnv(engine->getInteractionFrame());
        cont->handleValue(iEnv);
    }
}

void scam::applyEval(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * name = "eval";

    ObjectParameter p0;
    EnvParameter    p1;
    if ( argsToParms(args, engine, name, p0, p1) ) {
        Env * env = asEnv(p1.value);
        eval(p0.value, cont, env, engine);
    }
}

namespace
{
    void addForms(ScamEngine * engine, Env * env)
    {
        env->merge(initializeSchemeBase(engine, env));
        env->merge(initializeSchemeChar(engine, env));
        env->merge(initializeSchemeEval(engine, env));
        env->merge(initializeSchemeInexact(engine, env));
        env->merge(initializeSchemeLoad(engine, env));
        env->merge(initializeSchemeRead(engine, env));
        env->merge(initializeSchemeRepl(engine, env));
        env->merge(initializeSchemeWrite(engine, env));
        env->merge(initializeScamBase(engine, env));
        env->merge(initializeScamBacktrack(engine, env));
        env->merge(initializeScamClass(engine, env));
        env->merge(initializeScamError(engine, env));
        env->merge(initializeScamUnify(engine, env));
        env->merge(initializeScamMisc(engine, env));
    }

    Env * initializeSchemeBase(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "%", applyMod, engine);
        addPrimitive(env, "*", applyMul, engine);
        addPrimitive(env, "+", applyAdd, engine);
        addPrimitive(env, "-", applySub, engine);
        addPrimitive(env, "/", applyDiv, engine);

        addPrimitive(env, "<>", applyNe, engine);
        addPrimitive(env, "=",  applyEq, engine);
        addPrimitive(env, "<",  applyLt, engine);
        addPrimitive(env, "<=", applyLe, engine);
        addPrimitive(env, ">",  applyGt, engine);
        addPrimitive(env, ">=", applyGe, engine);

        addSpecialForm(env, "apply", applyApply, engine);
        addPrimitive(env, "append", applyAppend, engine);

        addPrimitive(env, "boolean?", applyBoolP, engine);

        addSpecialForm(env, "call/cc", applyCallCC, engine);
        addPrimitive(env, "car", applyCar, engine);
        addPrimitive(env, "cdr", applyCdr, engine);
        addPrimitive(env, "char?", applyCharP, engine);
        addPrimitive(env, "complex?", applyComplexP, engine);
        addPrimitive(env, "cons", applyCons, engine);

        addPrimitive(env, "eof-object", applyEofObject, engine);
        addPrimitive(env, "eof-object?", applyEofP, engine);
        addPrimitive(env, "eq?", applyEqP, engine);
        addPrimitive(env, "equal?", applyEqualP, engine);
        addPrimitive(env, "eqv?", applyEqvP, engine);
        addPrimitive(env, "error", applyError, engine);
        addPrimitive(env, "error-object-irritants", applyErrorIrritant, engine);
        addPrimitive(env, "error-object-message", applyErrorMessage, engine);
        addPrimitive(env, "error-object?", applyErrorP, engine);
        addPrimitive(env, "exact?", applyExactP, engine);

        addPrimitive(env, "file-error?", applyFileErrorP, engine);

        addPrimitive(env, "get-output-string",    applyGetOutStr,   engine);

        addPrimitive(env, "inexact?", applyInexactP, engine);
        addPrimitive(env, "integer?", applyIntegerP, engine);

        addPrimitive(env, "list", applyList, engine);
        addPrimitive(env, "list->string", applyList2String, engine);
        addPrimitive(env, "list?", applyListP, engine);

        addPrimitive(env, "make-list", applyMakeList, engine);
        addPrimitive(env, "make-string", applyMakeString, engine);

        addPrimitive(env, "newline", applyNewline, engine);
        addSpecialForm(env, "not", applyNot, engine);
        addPrimitive(env, "null?", applyNullP, engine);

        addPrimitive(env, "open-input-string", applyOpenInStr, engine);
        addPrimitive(env, "open-output-string", applyOpenOutStr, engine);

        addPrimitive(env, "pair?", applyPairP, engine);
        addPrimitive(env, "port?", applyPortP, engine);
        addPrimitive(env, "procedure?", applyProcedureP, engine);

        addPrimitive(env, "raise", applyRaise, engine);
        addPrimitive(env, "rational?", applyRationalP, engine);
        addPrimitive(env, "read-error?", applyReadErrorP, engine);
        addPrimitive(env, "real?", applyRealP, engine);

        addPrimitive(env, "set-car!", applySetCarX, engine);
        addPrimitive(env, "set-cdr!", applySetCdrX, engine);
        addPrimitive(env, "string", applyString, engine);
        addPrimitive(env, "string->list", applyString2List, engine);
        addPrimitive(env, "string-append", applyStringAppend, engine);
        addPrimitive(env, "string-copy!", applyStringCopyX, engine);
        addPrimitive(env, "string-copy", applyStringCopy, engine);
        addPrimitive(env, "string-fill!", applyStringFillX, engine);
        addPrimitive(env, "string-length", applyStringLength, engine);
        addPrimitive(env, "string-ref", applyStringRef, engine);
        addPrimitive(env, "string-set!", applyStringSetX, engine);
        addPrimitive(env, "string<=?", applyStringLeP, engine);
        addPrimitive(env, "string<?", applyStringLtP, engine);
        addPrimitive(env, "string=?", applyStringEqP, engine);
        addPrimitive(env, "string>=?", applyStringGeP, engine);
        addPrimitive(env, "string>?", applyStringGtP, engine);
        addPrimitive(env, "string?", applyStringP, engine);
        addPrimitive(env, "symbol?", applySymbolP, engine);

        addPrimitive(env, "vector?", applyVectorP, engine);

        addPrimitive(env, "with-exception-handler", applyWithHandler, engine);

	ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("base"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeChar(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "string-ci<=?", applyStringCiLeP, engine);
        addPrimitive(env, "string-ci<?", applyStringCiLtP, engine);
        addPrimitive(env, "string-ci=?", applyStringCiEqP, engine);
        addPrimitive(env, "string-ci>=?", applyStringCiGeP, engine);
        addPrimitive(env, "string-ci>?", applyStringCiGtP, engine);
        addPrimitive(env, "string-downcase", applyStringDowncase, engine);
        addPrimitive(env, "string-upcase", applyStringUpcase, engine);

	ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("char"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeEval(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "eval", applyEval, engine);

	ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("eval"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeInexact(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "finite?", applyFiniteP, engine);
        addPrimitive(env, "infinite?", applyInfiniteP, engine);
        addPrimitive(env, "nan?", applyNanP, engine);

	ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("inexact"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeLoad(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "load", applyLoad, engine);

	ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("load"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeRead(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "read", applyRead, engine);

	ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("read"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeRepl(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env,
                     "interaction-environment",
                     applyInteractionEnv,
                     engine);

	ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("repl"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeSchemeWrite(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "display", applyDisplay, engine);

	ScamValue name = makeList(makeSymbol("scheme"), makeSymbol("write"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeScamBase(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "dict?", applyDictP, engine);
        addPrimitive(env, "keyword?", applyKeywordP, engine);
        addPrimitive(env, "numeric?", applyNumericP, engine);
        addPrimitive(env, "vlen", applyVLen, engine);
        addPrimitive(env, "vref", applyVRef, engine);

	ScamValue name = makeList(makeSymbol("scam"), makeSymbol("base"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeScamBacktrack(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addSpecialForm(env, "amb", applyAmb, engine);
        addPrimitive(env, "backtrack", applyBacktrack, engine);

	ScamValue name = makeList(makeSymbol("scam"), makeSymbol("backtrack"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeScamClass(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addSpecialForm(env, "make-class", applyClassMaker, engine);
        addPrimitive(env, "class?", applyClassP, engine);
        addPrimitive(env, "instance?", applyInstanceP, engine);

	ScamValue name = makeList(makeSymbol("scam"), makeSymbol("class"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeScamError(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "make-error", applyMakeError, engine);
        addPrimitive(env, "error-category", applyErrorCat, engine);
        addPrimitive(env, "error->string", applyError2String, engine);

	ScamValue name = makeList(makeSymbol("scam"), makeSymbol("error"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeScamUnify(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addPrimitive(env, "match", applyMatch, engine);
        addPrimitive(env, "unify", applyUnify, engine);
        addPrimitive(env, "substitute", applySubstitute, engine);
        addPrimitive(env, "instantiate", applyInstantiate, engine);

	ScamValue name = makeList(makeSymbol("scam"), makeSymbol("unify"));
	engine->saveLibrary(name, env);
        return env;
    }

    Env * initializeScamMisc(ScamEngine * engine, Env * base)
    {
        Env * env = base->extend();

        addSpecialForm(env, "syntax-expand", applySyntaxExpand, engine);
        addSpecialForm(env, "undefine", applyUndefine, engine);
        addPrimitive(env, "environment?",  applyEnvironmentP, engine);
        addPrimitive(env, "spawn", applySpawn, engine);

	ScamValue name = makeList(makeSymbol("scam"), makeSymbol("misc"));
	engine->saveLibrary(name, env);
        return env;
    }

    void addSpecialForm(Env * env,
                        const char * name,
                        SfFunction func,
                        ScamEngine * engine)
    {
        ScamValue key  = makeSymbol(name);
        ScamValue form = makeSpecialForm(name, func, engine, true);
        doPut(env, key, form);
    }

    void addPrimitive(Env * env,
                      const char * name,
                      PrimFunction func,
                      ScamEngine * engine)
    {
        ScamValue key  = makeSymbol(name);
        ScamValue form = makePrimitive(name, func, engine, true);
        doPut(env, key, form);
    }

    void addPorts(Env * env, ScamEngine * engine)
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
}
