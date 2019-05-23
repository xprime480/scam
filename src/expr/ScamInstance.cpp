#include "expr/ScamInstance.hpp"

#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/InstanceCont.hpp"
#include "expr/TypePredicates.hpp"
#include "input/FunctionDefParser.hpp"
#include "input/SymbolPlusManyParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

#include "util/DebugTrace.hpp"

using namespace scam;
using namespace std;

namespace
{
    static ScamEnvKeyType self   =
        ExpressionFactory::makeSymbol("self", false);

    static ScamValue nil = ExpressionFactory::makeNil();
}

scam::ScamEnvKeyType ScamInstance::parent =
    ExpressionFactory::makeSymbol("parent", false);

ScamInstance::ScamInstance(const ScamClass * cls, Env * env)
    : ScamExpr(ScamData::Instance)
{
    INSTANCEPRIVENV(this) = standardMemoryManager.make<Env>();
    INSTANCELOCALENV(this) = env->extend();

    ScamClassAdapter adapter(cls);

    size_t var_count = adapter.getVarCount();
    for ( size_t n = 0 ; n < var_count ; ++n ) {
        const ScamSymbol * var = adapter.getVar(n);
        INSTANCELOCALENV(this)->put(var, nil);
    }

    size_t fun_count = adapter.getMethodCount();
    for ( size_t n = 0 ; n < fun_count ; ++n ) {
        const FunctionDefParser * fun = adapter.getMethod(n);

        const ScamSymbol * name = fun->getName();
        const LambdaParser * lambda = fun->getLambda();
        ScamValue impl = ExpressionFactory::makeClosure(lambda, INSTANCELOCALENV(this), false);

        INSTANCEPRIVENV(this)->put(name, impl);
    }
}

ScamInstance * ScamInstance::makeInstance(const ScamClass * cls, Env * env)
{
    return new ScamInstance(cls, env);
}

void ScamInstance::apply(ScamValue args, Continuation * cont, Env * env)
{
    InstanceParser * parser = standardMemoryManager.make<InstanceParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage("instance", "(sym forms*)", args, cont);
        return;
    }

    ScamEnvKeyType name = parser->getSymbol();
    ScamValue funargs = parser->getForms();

    Continuation * newCont =
        standardMemoryManager.make<InstanceCont>(this, name, cont);
    if ( TypePredicates::isNil(funargs) ) {
        newCont->run(funargs);
    }
    else {
        funargs->mapEval(newCont, env);
    }
}

void ScamInstance::setSelf(ScamValue expr) const
{
    INSTANCELOCALENV(this)->put(self, expr);
}

void ScamInstance::setParent(ScamValue expr) const
{
    INSTANCELOCALENV(this)->put(parent, expr);
}
