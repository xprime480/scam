#include "expr/ScamInstance.hpp"

#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/InstanceCont.hpp"
#include "input/FunctionDefParser.hpp"
#include "input/SymbolPlusManyParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

namespace
{
    static ScamEnvKeyType self   =
        ExpressionFactory::makeSymbol("self", false);

    static ExprHandle nil = ExpressionFactory::makeNil();
}

scam::ScamEnvKeyType ScamInstance::parent =
    ExpressionFactory::makeSymbol("parent", false);

ScamInstance::ScamInstance(const ScamClass * cls, Env * env)
    : priv(standardMemoryManager.make<Env>())
    , local(env->extend())
{
    data.type = ScamData::Instance;

    ScamClassAdapter adapter(cls);

    size_t var_count = adapter.getVarCount();
    for ( size_t n = 0 ; n < var_count ; ++n ) {
        const ScamSymbol * var = adapter.getVar(n);
        local->put(var, nil);
    }

    size_t fun_count = adapter.getMethodCount();
    for ( size_t n = 0 ; n < fun_count ; ++n ) {
        const FunctionDefParser * fun = adapter.getMethod(n);

        const ScamSymbol * name = fun->getName();
        const LambdaParser * lambda = fun->getLambda();
        ExprHandle impl = ExpressionFactory::makeClosure(lambda, local, false);

        priv->put(name, impl);
    }
}

ScamInstance * ScamInstance::makeInstance(const ScamClass * cls, Env * env)
{
    return new ScamInstance(cls, env);
}

void ScamInstance::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        local->mark();
        priv->mark();
    }
}

string ScamInstance::toString() const
{
    return "instance";
}

bool ScamInstance::hasApply() const
{
    return true;
}

void ScamInstance::apply(ExprHandle args, Continuation * cont, Env * env)
{
    InstanceParser * parser = standardMemoryManager.make<InstanceParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage("instance", "(sym forms*)", args, cont);
        return;
    }

    ScamEnvKeyType name = parser->getSymbol();
    ExprHandle funargs = parser->getForms();

    Continuation * newCont =
        standardMemoryManager.make<InstanceCont>(this, name, cont);
    funargs->mapEval(newCont, env);
}

void ScamInstance::setSelf(ExprHandle expr) const
{
    local->put(self, expr);
}

void ScamInstance::setParent(ExprHandle expr) const
{
    local->put(parent, expr);
}
