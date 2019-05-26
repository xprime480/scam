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
    CLOSUREDEF(this) = parser;
    CLOSUREENV(this) = env;
    MACROLIKE(this) = macrolike;
}

ScamClosure * ScamClosure::makeInstance(const LambdaParser * parser,
                                        Env * env,
                                        bool macrolike)
{
    return new ScamClosure(parser, env, macrolike);
}

void ScamClosure::apply(ScamValue args, Continuation * cont, Env * env)
{
    workQueueHelper<ClosureWorker>(CLOSUREDEF(this),
                                   CLOSUREENV(this),
                                   cont,
                                   args,
                                   env,
                                   MACROLIKE(this));
}

