#include "ScamEngine.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "input/ScamParser.hpp"
#include "output/OutputHandler.hpp"

using namespace std;
using namespace scam;

ScamEngine::ScamEngine(bool initEnv)
{
    reset(initEnv);
}

void ScamEngine::reset(bool initEnv)
{
    Env newEnv;
    env = newEnv;

    if ( initEnv ) {
        getStandardEnv();
    }
}

void ScamEngine::pushFrame()
{
    env = env.extend();
}

Env ScamEngine::getFrame()
{
    return env;
}

void ScamEngine::popFrame()
{
    env = env.parent();
}

void ScamEngine::addBinding(ScamExpr * key, ScamExpr * val)
{
    env.put(key, val);
}

bool ScamEngine::hasBinding(ScamExpr * key)
{
    return env.check(key);
}

ExprHandle ScamEngine::getBinding(ScamExpr * key, bool top)
{
    Env temp = top ? env.top() : env;
    return temp.get(key);
}

void ScamEngine::rebind(ScamExpr * key, ScamExpr * val)
{
    env.assign(key, val);
}

void ScamEngine::pushInput(Tokenizer & tokenizer)
{
    shared_ptr<ScamParser> p = make_shared<ScamParser>(tokenizer);
    input.push_back(p);
}

void ScamEngine::popInput()
{
    if ( ! input.empty() ) {
        input.pop_back();
    }
}

ExprHandle ScamEngine::read()
{
    if ( input.empty() ) {
        return ExpressionFactory::makeNull();
    }

    shared_ptr<ScamParser> p = input.back();
    return p->parseExpr();
}

void ScamEngine::eval(ScamExpr * expr, ContHandle cont)
{
    expr->eval(cont, env);
}

void ScamEngine::apply(ScamExpr * expr, ScamExpr * args, ContHandle cont)
{
    expr->apply(args, cont, env);
}
