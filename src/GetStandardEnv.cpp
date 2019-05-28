#include "ScamEngine.hpp"

#include "Env.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AllSpecialForms.hpp"
#include "prim/AllPrimitives.hpp"

using namespace scam;
using namespace std;

namespace
{
    template <typename T, typename... Args>
    void addForm(Env * env, char const * name, Args... args)
    {
        ScamValue key  = makeSymbol(name);
        ScamValue form = makeForm<T>(args...);
        env->put(key, form);
    }

    void addSpecialForm(Env * env,
                        const char * name,
                        SfFunction func,
                        ScamEngine * engine)
    {
        ScamValue key  = makeSymbol(name);
        ScamValue form = makeSpecialForm(name, func, engine, true);
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

    addForm<Match>(env, "match");
    addForm<Unify>(env, "unify");
    addForm<Substitute>(env, "substitute");
    addForm<Instantiate>(env, "instantiate");

    addForm<Add>(env, "+");
    addForm<Sub>(env, "-");
    addForm<Mul>(env, "*");
    addForm<Div>(env, "/");
    addForm<Mod>(env, "%");

    addForm<Eq>(env, "=");
    addForm<Ne>(env, "<>");
    addForm<Lt>(env, "<");
    addForm<Le>(env, "<=");
    addForm<Gt>(env, ">");
    addForm<Ge>(env, ">=");
    addForm<EqualP>(env, "eq?");

    addForm<List>(env, "list");
    addForm<Cons>(env, "cons");
    addForm<Car>(env, "car");
    addForm<Cdr>(env, "cdr");

    addForm<VLen>(env, "vlen");
    addForm<VRef>(env, "vref");

    addForm<NilP>(env, "nil?");
    addForm<ErrorP>(env, "error?");
    addForm<ConsP>(env, "cons?");
    addForm<ListP>(env, "list?");
    addForm<VectorP>(env, "vector?");
    addForm<BoolP>(env, "bool?");
    addForm<CharP>(env, "char?");
    addForm<StringP>(env, "string?");
    addForm<SymbolP>(env, "symbol?");
    addForm<KeywordP>(env, "keyword?");

    addForm<NumericP>(env, "numeric?");
    addForm<ComplexP>(env, "complex?");
    addForm<RealP>(env, "real?");
    addForm<RationalP>(env, "rational?");
    addForm<IntegerP>(env, "integer?");
    addForm<ExactP>(env, "exact?");

    addForm<ProcP>(env, "proc?");
    addForm<ClassP>(env, "class?");
    addForm<InstanceP>(env, "instance?");
    addForm<DictP>(env, "dict?");

    addForm<Begin>(env, "begin");

    addForm<Load>(env, "load", this);
    //    addForm<Include>(env, "include", this);
    addForm<Spawn>(env, "spawn");
    addForm<Error>(env, "error");
    addForm<Backtrack>(env, "backtrack", this);
    addForm<Trace>(env, "trace");
}
