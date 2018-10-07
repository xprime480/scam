
#include "ScamEngine.hpp"

#include "expr/ExpressionFactory.hpp"
#include "form/Quote.hpp"
#include "prim/Add.hpp"

using namespace scam;
using namespace std;

namespace
{
    template <typename T>
    void addForm(Env & env, char const * name)
    {
        shared_ptr<ScamExpr> sym = ExpressionFactory::makeSymbol(name);
        shared_ptr<ScamExpr> form = ExpressionFactory::makeForm<T>();
        env.put(sym, form);
    }
}

Env ScamEngine::getStandardEnv()
{
    Env env;

    addForm<Quote>(env, "quote");

    addForm<Add>(env, "+");

    return env;
}
