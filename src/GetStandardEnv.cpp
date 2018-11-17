
#include "ScamEngine.hpp"

#include "expr/ExpressionFactory.hpp"
#include "form/AllSpecialForms.hpp"
#include "prim/AllPrimitives.hpp"

using namespace scam;
using namespace std;

namespace
{
    template <typename T, typename... Args>
    void addForm(Env env, char const * name, Args... args)
    {
        ExprHandle sym = ExpressionFactory::makeSymbol(name);
        ExprHandle form = ExpressionFactory::makeForm<T>(args...);
        env.put(sym.get(), form.get());
    }
}

void ScamEngine::getStandardEnv()
{
    addForm<Assign>(env, "assign!");
    addForm<Define>(env, "define");
    addForm<Lambda>(env, "lambda");
    addForm<QuasiQuote>(env, "quasiquote");
    addForm<Quote>(env, "quote");
    addForm<Macro>(env, "macro");
    addForm<Let>(env, "let");
    addForm<LetStar>(env, "let*");
    addForm<LetRec>(env, "letrec");
    addForm<Eval>(env, "eval");
    addForm<ClassMaker>(env, "make-class");
    addForm<CallCC>(env, "call/cc");

    addForm<If>(env, "if");
    addForm<And>(env, "and");
    addForm<Or>(env, "or");
    addForm<Not>(env, "not");

    addForm<Add>(env, "+");
    addForm<Sub>(env, "-");
    addForm<Mul>(env, "*");
    addForm<Div>(env, "/");

    addForm<Eq>(env, "=");
    addForm<Ne>(env, "<>");
    addForm<Lt>(env, "<");
    addForm<Le>(env, "<=");
    addForm<Gt>(env, ">");
    addForm<Ge>(env, ">=");

    addForm<List>(env, "list");
    addForm<Cons>(env, "cons");
    addForm<Car>(env, "car");
    addForm<Cdr>(env, "cdr");

    addForm<NilP>(env, "nil?");

    addForm<Progn>(env, "progn");

    addForm<Load>(env, "load", this);
}
