#include "ScamEngine.hpp"

#include "Env.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AllSpecialForms.hpp"
#include "prim/AllPrimitives.hpp"

using namespace scam;
using namespace std;

namespace
{
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
}

void ScamEngine::getStandardEnv()
{
    addSpecialForm(env, "assign!", applyAssign, this);
    addSpecialForm(env, "define", applyDefine, this);
    addSpecialForm(env, "undefine", applyUndefine, this);
    addSpecialForm(env, "lambda", applyLambda, this);
    addSpecialForm(env, "quasiquote", applyQuasiQuote, this);
    addSpecialForm(env, "quote", applyQuote, this);
    addSpecialForm(env, "macro", applyMacro, this);
    addSpecialForm(env, "let", applyLet, this);
    addSpecialForm(env, "let*", applyLetStar, this);
    addSpecialForm(env, "letrec", applyLetRec, this);
    addSpecialForm(env, "eval", applyEval, this);
    addSpecialForm(env, "apply", applyApply, this);
    addSpecialForm(env, "make-class", applyClassMaker, this);
    addSpecialForm(env, "call/cc", applyCallCC, this);
    addSpecialForm(env, "amb", applyAmb, this);

    addSpecialForm(env, "if", applyIf, this);
    addSpecialForm(env, "and", applyAnd, this);
    addSpecialForm(env, "or", applyOr, this);
    addSpecialForm(env, "not", applyNot, this);

    addPrimitive(env, "match", applyMatch, this);
    addPrimitive(env, "unify", applyUnify, this);
    addPrimitive(env, "substitute", applySubstitute, this);
    addPrimitive(env, "instantiate", applyInstantiate, this);

    addPrimitive(env, "+", applyAdd, this);
    addPrimitive(env, "-", applySub, this);
    addPrimitive(env, "*", applyMul, this);
    addPrimitive(env, "/", applyDiv, this);
    addPrimitive(env, "%", applyMod, this);

    addPrimitive(env, "=", applyEq, this);
    addPrimitive(env, "<>", applyNe, this);
    addPrimitive(env, "<", applyLt, this);
    addPrimitive(env, "<=", applyLe, this);
    addPrimitive(env, ">", applyGt, this);
    addPrimitive(env, ">=", applyGe, this);

    addPrimitive(env, "eq?",    applyEqP,    this);
    addPrimitive(env, "eqv?",   applyEqvP,   this);
    addPrimitive(env, "equal?", applyEqualP, this);

    addPrimitive(env, "vlen", applyVLen, this);
    addPrimitive(env, "vref", applyVRef, this);

    addTypePredicates(env, this);

    addPrimitive(env, "begin", applyBegin, this);

    addPrimitive(env, "load", applyLoad, this);
    addPrimitive(env, "spawn", applySpawn, this);
    addPrimitive(env, "backtrack", applyBacktrack, this);
    addPrimitive(env, "trace", applyTrace, this);

    addStringOps(env, this);
    addPairOps(env, this);
    addListOps(env, this);
    addErrorOps(env, this);
    addPortOps(env, this);
    addInputOps(env, this);
}

namespace
{
    void addSpecialForm(Env * env,
                        const char * name,
                        SfFunction func,
                        ScamEngine * engine)
    {
        ScamValue key  = makeSymbol(name);
        ScamValue form = makeSpecialForm(name, func, engine, true);
        env->put(key, form);
    }

    void addPrimitive(Env * env,
                      const char * name,
                      PrimFunction func,
                      ScamEngine * engine)
    {
        ScamValue key  = makeSymbol(name);
        ScamValue form = makePrimitive(name, func, engine, true);
        env->put(key, form);
    }

    void addTypePredicates(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "null?",         applyNullP,      engine);
        addPrimitive(env, "error-object?", applyErrorP,     engine);
        addPrimitive(env, "pair?",         applyPairP,      engine);
        addPrimitive(env, "list?",         applyListP,      engine);
        addPrimitive(env, "vector?",       applyVectorP,    engine);
        addPrimitive(env, "boolean?",      applyBoolP,      engine);
        addPrimitive(env, "char?",         applyCharP,      engine);
        addPrimitive(env, "string?",       applyStringP,    engine);
        addPrimitive(env, "symbol?",       applySymbolP,    engine);
        addPrimitive(env, "keyword?",      applyKeywordP,   engine);

        addPrimitive(env, "numeric?",      applyNumericP,   engine);
        addPrimitive(env, "complex?",      applyComplexP,   engine);
        addPrimitive(env, "real?",         applyRealP,      engine);
        addPrimitive(env, "rational?",     applyRationalP,  engine);
        addPrimitive(env, "integer?",      applyIntegerP,   engine);
        addPrimitive(env, "exact?",        applyExactP,     engine);

        addPrimitive(env, "procedure?",    applyProcedureP, engine);
        addPrimitive(env, "class?",        applyClassP,     engine);
        addPrimitive(env, "instance?",     applyInstanceP,  engine);
        addPrimitive(env, "dict?",         applyDictP,      engine);

        addPrimitive(env, "port?",         applyPortP,      engine);
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
    }

    void addPortOps(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "open-input-string",    applyOpenInStr,   engine);
    }

    void addInputOps(Env * env, ScamEngine * engine)
    {
        addPrimitive(env, "read",    applyRead,   engine);
    }
}
