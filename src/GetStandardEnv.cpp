
#include "ScamEngine.hpp"

#include "expr/ExpressionFactory.hpp"
#include "form/AllSpecialForms.hpp"
#include "prim/AllPrimitives.hpp"

using namespace scam;
using namespace std;

namespace
{
    template <typename T, typename... Args>
    void addForm(Env & env, char const * name, Args... args)
    {
        ExprHandle sym = ExpressionFactory::makeSymbol(name);
        ExprHandle form = ExpressionFactory::makeForm<T>(args...);
        env.put(sym.get(), form.get());
    }
}

void ScamEngine::getStandardEnv()
{
    addForm<Assign>(env, "assign!", this);
    addForm<Define>(env, "define", this);
    addForm<Undefine>(env, "undefine", this);
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

    addForm<NilP>(env, "nil?");
    addForm<ConsP>(env, "cons?");
    addForm<ListP>(env, "list?");
    addForm<VectorP>(env, "vector?");
    addForm<BoolP>(env, "bool?");
    addForm<CharP>(env, "char?");
    addForm<StringP>(env, "string?");
    addForm<SymbolP>(env, "symbol?");
    addForm<NumericP>(env, "numeric?");
    addForm<FloatP>(env, "float?");
    addForm<IntegerP>(env, "integer?");
    addForm<ProcP>(env, "proc?");
    addForm<ClassP>(env, "class?");
    addForm<InstanceP>(env, "instance?");

    addForm<Progn>(env, "progn");
    addForm<Progn>(env, "begin");

    addForm<Load>(env, "load", this);
    addForm<Spawn>(env, "spawn");
    addForm<Error>(env, "error");
    addForm<Backtrack>(env, "backtrack", this);
}
