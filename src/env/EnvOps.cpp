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

    extern void addSpecialForm(Env * env,
                               const char * name,
                               SfFunction func,
                               ScamEngine * engine);

    extern void addPrimitive(Env * env,
                             const char * name,
                             PrimFunction func,
                             ScamEngine * engine);

    extern void addTypePredicates(Env * env, ScamEngine * engine);

    extern void addStringOps(Env * env, ScamEngine * engine);
    extern void addPairOps(Env * env, ScamEngine * engine);
    extern void addListOps(Env * env, ScamEngine * engine);
    extern void addErrorOps(Env * env, ScamEngine * engine);
    extern void addPortOps(Env * env, ScamEngine * engine);
    extern void addInputOps(Env * env, ScamEngine * engine);
    extern void addOutputOps(Env * env, ScamEngine * engine);

    extern void addPorts(Env * env, ScamEngine * engine);

    extern void doPut(Env * env, ScamValue key, ScamValue value);
}

Env * scam::getConfigurationEnv(ScamEngine * engine)
{
    Env * env = standardMemoryManager.make<Env>();
    addPorts(env, engine);
    return env;
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
        addSpecialForm(env, "set!", applySetX, engine);
        addSpecialForm(env, "define", applyDefine, engine);
        addSpecialForm(env, "undefine", applyUndefine, engine);
        addSpecialForm(env, "lambda", applyLambda, engine);
        addSpecialForm(env, "quasiquote", applyQuasiQuote, engine);
        addSpecialForm(env, "quote", applyQuote, engine);
        addSpecialForm(env, "define-syntax", applyDefineSyntax, engine);
        addSpecialForm(env, "syntax-expand", applySyntaxExpand, engine);
        addSpecialForm(env, "let", applyLet, engine);
        addSpecialForm(env, "let*", applyLetStar, engine);
        addSpecialForm(env, "letrec", applyLetRec, engine);

        addPrimitive(env, "eval", applyEval, engine);
        addPrimitive(env,
                     "interaction-environment",
                     applyInteractionEnv,
                     engine);

        addSpecialForm(env, "apply", applyApply, engine);
        addSpecialForm(env, "make-class", applyClassMaker, engine);
        addSpecialForm(env, "call/cc", applyCallCC, engine);
        addSpecialForm(env, "amb", applyAmb, engine);

        addSpecialForm(env, "if", applyIf, engine);
        addSpecialForm(env, "and", applyAnd, engine);
        addSpecialForm(env, "or", applyOr, engine);
        addSpecialForm(env, "not", applyNot, engine);

        addPrimitive(env, "match", applyMatch, engine);
        addPrimitive(env, "unify", applyUnify, engine);
        addPrimitive(env, "substitute", applySubstitute, engine);
        addPrimitive(env, "instantiate", applyInstantiate, engine);

        addPrimitive(env, "+", applyAdd, engine);
        addPrimitive(env, "-", applySub, engine);
        addPrimitive(env, "*", applyMul, engine);
        addPrimitive(env, "/", applyDiv, engine);
        addPrimitive(env, "%", applyMod, engine);

        addPrimitive(env, "=", applyEq, engine);
        addPrimitive(env, "<>", applyNe, engine);
        addPrimitive(env, "<", applyLt, engine);
        addPrimitive(env, "<=", applyLe, engine);
        addPrimitive(env, ">", applyGt, engine);
        addPrimitive(env, ">=", applyGe, engine);

        addPrimitive(env, "eq?",    applyEqP,    engine);
        addPrimitive(env, "eqv?",   applyEqvP,   engine);
        addPrimitive(env, "equal?", applyEqualP, engine);

        addPrimitive(env, "vlen", applyVLen, engine);
        addPrimitive(env, "vref", applyVRef, engine);

        addTypePredicates(env, engine);

        addPrimitive(env, "begin", applyBegin, engine);

        addPrimitive(env, "load", applyLoad, engine);
        addPrimitive(env, "spawn", applySpawn, engine);
        addPrimitive(env, "backtrack", applyBacktrack, engine);

        addStringOps(env, engine);
        addPairOps(env, engine);
        addListOps(env, engine);
        addErrorOps(env, engine);
        addPortOps(env, engine);
        addInputOps(env, engine);
        addOutputOps(env, engine);
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

    void addTypePredicates(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "null?",         applyNullP,        engine);
        addPrimitive(env, "error-object?", applyErrorP,       engine);
        addPrimitive(env, "pair?",         applyPairP,        engine);
        addPrimitive(env, "list?",         applyListP,        engine);
        addPrimitive(env, "vector?",       applyVectorP,      engine);
        addPrimitive(env, "boolean?",      applyBoolP,        engine);
        addPrimitive(env, "char?",         applyCharP,        engine);
        addPrimitive(env, "string?",       applyStringP,      engine);
        addPrimitive(env, "symbol?",       applySymbolP,      engine);
        addPrimitive(env, "keyword?",      applyKeywordP,     engine);

        addPrimitive(env, "numeric?",      applyNumericP,     engine);
        addPrimitive(env, "complex?",      applyComplexP,     engine);
        addPrimitive(env, "real?",         applyRealP,        engine);
        addPrimitive(env, "rational?",     applyRationalP,    engine);
        addPrimitive(env, "integer?",      applyIntegerP,     engine);
        addPrimitive(env, "exact?",        applyExactP,       engine);
        addPrimitive(env, "inexact?",      applyInexactP,     engine);
        addPrimitive(env, "nan?",          applyNanP,         engine);
        addPrimitive(env, "finite?",       applyFiniteP,      engine);
        addPrimitive(env, "infinite?",     applyInfiniteP,    engine);

        addPrimitive(env, "procedure?",    applyProcedureP,   engine);
        addPrimitive(env, "class?",        applyClassP,       engine);
        addPrimitive(env, "instance?",     applyInstanceP,    engine);
        addPrimitive(env, "dict?",         applyDictP,        engine);

        addPrimitive(env, "port?",         applyPortP,        engine);
        addPrimitive(env, "eof-object?",   applyEofP,         engine);
        addPrimitive(env, "environment?",  applyEnvironmentP, engine);
    }

    void addStringOps(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "make-string",     applyMakeString,     engine);
        addPrimitive(env, "string",          applyString,         engine);
        addPrimitive(env, "string-length",   applyStringLength,   engine);
        addPrimitive(env, "string-ref",      applyStringRef,      engine);
        addPrimitive(env, "string-set!",     applyStringSetX,     engine);
        addPrimitive(env, "string=?",        applyStringEqP,      engine);
        addPrimitive(env, "string-ci=?",     applyStringCiEqP,    engine);
        addPrimitive(env, "string<?",        applyStringLtP,      engine);
        addPrimitive(env, "string-ci<?",     applyStringCiLtP,    engine);
        addPrimitive(env, "string<=?",       applyStringLeP,      engine);
        addPrimitive(env, "string-ci<=?",    applyStringCiLeP,    engine);
        addPrimitive(env, "string>?",        applyStringGtP,      engine);
        addPrimitive(env, "string-ci>?",     applyStringCiGtP,    engine);
        addPrimitive(env, "string>=?",       applyStringGeP,      engine);
        addPrimitive(env, "string-ci>=?",    applyStringCiGeP,    engine);
        addPrimitive(env, "string-upcase",   applyStringUpcase,   engine);
        addPrimitive(env, "string-downcase", applyStringDowncase, engine);
        addPrimitive(env, "string-append",   applyStringAppend,   engine);
        addPrimitive(env, "string->list",    applyString2List,    engine);
        addPrimitive(env, "list->string",    applyList2String,    engine);
        addPrimitive(env, "string-copy",     applyStringCopy,     engine);
        addPrimitive(env, "string-copy!",    applyStringCopyX,    engine);
        addPrimitive(env, "string-fill!",    applyStringFillX,    engine);
    }

    void addPairOps(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "cons",     applyCons,    engine);
        addPrimitive(env, "car",      applyCar,     engine);
        addPrimitive(env, "cdr",      applyCdr,     engine);
        addPrimitive(env, "set-car!", applySetCarX, engine);
        addPrimitive(env, "set-cdr!", applySetCdrX, engine);
    }

    void addListOps(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "make-list", applyMakeList, engine);
        addPrimitive(env, "list",      applyList,     engine);
        addPrimitive(env, "append",    applyAppend,   engine);
    }

    void addErrorOps(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "make-error",             applyMakeError,     engine);
        addPrimitive(env, "error",                  applyError,         engine);
        addPrimitive(env, "raise",                  applyRaise,         engine);
        addPrimitive(env, "with-exception-handler", applyWithHandler,   engine);
        addPrimitive(env, "error-object-message",   applyErrorMessage,  engine);
        addPrimitive(env, "error-object-irritants", applyErrorIrritant, engine);
        addPrimitive(env, "read-error?",            applyReadErrorP,    engine);
        addPrimitive(env, "file-error?",            applyFileErrorP,    engine);
        addPrimitive(env, "error-category",         applyErrorCat,      engine);
        addPrimitive(env, "error->string",          applyError2String,  engine);
    }

    void addPortOps(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "open-input-string",    applyOpenInStr,   engine);
        addPrimitive(env, "open-output-string",   applyOpenOutStr,  engine);
        addPrimitive(env, "get-output-string",    applyGetOutStr,   engine);
    }

    void addInputOps(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "read",       applyRead,      engine);
        addPrimitive(env, "eof-object", applyEofObject, engine);
    }

    void addOutputOps(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "display",       applyDisplay,      engine);
        addPrimitive(env, "newline",       applyNewline,      engine);
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
