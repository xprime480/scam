#include "expr/ScamClosure.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ClosureWorker.hpp"
#include "input/ParameterListParser.hpp"

#include <sstream>

using namespace scam;
using namespace std;

#define CLOSUREDEF(data) ((data).value.closureData.parser)
#define CLOSUREENV(data) ((data).value.closureData.env)
#define MACROLIKE(data) ((data).value.closureData.macrolike)

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

string ScamClosure::toString() const
{
    stringstream s;
    s << "(";

    if ( MACROLIKE(data) ) {
        s << "macro ";
    }
    else {
        s << "lambda ";
    }
    s << CLOSUREDEF(data)->getArgs()->getValue()->toString();
    s << " ";

    const size_t count = CLOSUREDEF(data)->getFormCount();
    for ( size_t idx = 0 ; idx < count ; ++ idx ) {
        if ( idx > 0 ) {
            s << " ";
        }
        s << CLOSUREDEF(data)->getForm(idx)->toString();
    }

    s << ")";
    return s.str();
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
