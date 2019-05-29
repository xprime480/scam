#include "ScamEngine.hpp"

#include "Env.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AllSpecialForms.hpp"
#include "prim/AllPrimitives.hpp"

using namespace scam;
using namespace std;

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
    addPrimitive(env, "eq?", applyEqualP, this);

    addPrimitive(env, "list", applyList, this);
    addPrimitive(env, "cons", applyCons, this);
    addPrimitive(env, "car", applyCar, this);
    addPrimitive(env, "cdr", applyCdr, this);

    addPrimitive(env, "vlen", applyVLen, this);
    addPrimitive(env, "vref", applyVRef, this);

    addPrimitive(env, "nil?", applyNilP, this);
    addPrimitive(env, "error?", applyErrorP, this);
    addPrimitive(env, "cons?", applyConsP, this);
    addPrimitive(env, "list?", applyListP, this);
    addPrimitive(env, "vector?", applyVectorP, this);
    addPrimitive(env, "bool?", applyBoolP, this);
    addPrimitive(env, "char?", applyCharP, this);
    addPrimitive(env, "string?", applyStringP, this);
    addPrimitive(env, "symbol?", applySymbolP, this);
    addPrimitive(env, "keyword?", applyKeywordP, this);

    addPrimitive(env, "numeric?", applyNumericP, this);
    addPrimitive(env, "complex?", applyComplexP, this);
    addPrimitive(env, "real?", applyRealP, this);
    addPrimitive(env, "rational?", applyRationalP, this);
    addPrimitive(env, "integer?", applyIntegerP, this);
    addPrimitive(env, "exact?", applyExactP, this);

    addPrimitive(env, "proc?", applyProcP, this);
    addPrimitive(env, "class?", applyClassP, this);
    addPrimitive(env, "instance?", applyInstanceP, this);
    addPrimitive(env, "dict?", applyDictP, this);

    addPrimitive(env, "begin", applyBegin, this);

    addPrimitive(env, "load", applyLoad, this);
    addPrimitive(env, "spawn", applySpawn, this);
    addPrimitive(env, "error", applyError, this);
    addPrimitive(env, "backtrack", applyBacktrack, this);
    addPrimitive(env, "trace", applyTrace, this);
}
