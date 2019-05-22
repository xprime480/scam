#include "expr/ScamClosure.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ClosureWorker.hpp"
#include "input/ParameterListParser.hpp"

using namespace scam;
using namespace std;

ScamClosure::ScamClosure(const LambdaParser * parser, Env * env, bool macrolike)
    : ScamExpr(ScamData::Closure)
{
    CLOSUREDEF(data) = parser;
    CLOSUREENV(data) = env;
    MACROLIKE(data) = macrolike;
}

ScamClosure * ScamClosure::makeInstance(const LambdaParser * parser,
                                        Env * env,
                                        bool macrolike)
{
    return new ScamClosure(parser, env, macrolike);
}

void ScamClosure::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        CLOSUREDEF(data)->mark();
        CLOSUREENV(data)->mark();
    }
}

void ScamClosure::apply(ExprHandle args, Continuation * cont, Env * env)
{
    workQueueHelper<ClosureWorker>(CLOSUREDEF(data),
                                   CLOSUREENV(data),
                                   cont,
                                   args,
                                   env,
                                   MACROLIKE(data));
}

ExprHandle ScamClosure::withEnvUpdate(Env * updated) const
{
    return ExpressionFactory::makeClosure(CLOSUREDEF(data), updated);
}
