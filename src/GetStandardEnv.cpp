#include "ScamEngine.hpp"

#include "expr/ExpressionFactory.hpp"
#include "form/AllSpecialForms.hpp"
#include "prim/AllPrimitives.hpp"

using namespace scam;
using namespace std;

namespace
{
    template <typename T, typename... Args>
    void addForm(Env * env, char const * name, Args... args)
    {
        ScamEnvKeyType key = ExpressionFactory::makeSymbol(name);
        ExprHandle form = ExpressionFactory::makeForm<T>(args...);
        env->put(key, form);
    }
}

void ScamEngine::getStandardEnv()
{
    addForm<Assign>(env, "assign!", this);
    addForm<Define>(env, "define", this);
    addForm<Undefine>(env, "undefine");
    addForm<Lambda>(env, "lambda");
    addForm<QuasiQuote>(env, "quasiquote");
    addForm<Quote>(env, "quote");
    addForm<Macro>(env, "macro");
    addForm<Let>(env, "let");
    addForm<LetStar>(env, "let*", this);
    addForm<LetRec>(env, "letrec");
    addForm<Eval>(env, "eval");
    addForm<Apply>(env, "apply");
    addForm<ClassMaker>(env, "make-class");
    addForm<CallCC>(env, "call/cc");
    addForm<Amb>(env, "amb", this);

    addForm<If>(env, "if");
    addForm<And>(env, "and");
    addForm<Or>(env, "or");
    addForm<Not>(env, "not");

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
