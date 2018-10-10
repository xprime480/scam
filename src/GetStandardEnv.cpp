
#include "ScamEngine.hpp"

#include "expr/ExpressionFactory.hpp"
#include "form/AllSpecialForms.hpp"
#include "prim/AllPrimitives.hpp"

using namespace scam;
using namespace std;

namespace
{
    template <typename T>
    void addForm(Env & env, char const * name)
    {
        ExprHandle sym = ExpressionFactory::makeSymbol(name);
        ExprHandle form = ExpressionFactory::makeForm<T>();
        env.put(sym, form);
    }
}

Env ScamEngine::getStandardEnv()
{
    Env env;

    addForm<Quote>(env, "quote");
    addForm<Define>(env, "define");

    addForm<If>(env, "if");
    addForm<And>(env, "and");
    addForm<Or>(env, "or");
    addForm<Not>(env, "not");

    addForm<Add>(env, "+");
    addForm<Sub>(env, "-");
    addForm<Mul>(env, "*");
    addForm<Div>(env, "/");

    return env;
}
