#include "expr/ScamClosure.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ClosureWorker.hpp"
#include "input/ParameterListParser.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamClosure::ScamClosure(const LambdaParser * parser, Env * env, bool macrolike)
    : parser(parser)
    , env(env)
    , macrolike(macrolike)
{
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
        parser->mark();
        env->mark();
    }
}

string ScamClosure::toString() const
{
    stringstream s;
    s << "(";

    if ( macrolike ) {
        s << "macro ";
    }
    else {
        s << "lambda ";
    }
    s << parser->getArgs()->getValue()->toString();
    s << " ";

    const size_t count = parser->getFormCount();
    for ( size_t idx = 0 ; idx < count ; ++ idx ) {
        if ( idx > 0 ) {
            s << " ";
        }
        s << parser->getForm(idx)->toString();
    }

    s << ")";
    return s.str();
}

bool ScamClosure::hasApply() const
{
    return true;
}

void ScamClosure::apply(ExprHandle args, Continuation * cont, Env * env)
{
    workQueueHelper<ClosureWorker>(parser,
                                   this->env,
                                   cont,
                                   args,
                                   env,
                                   macrolike);
}

bool ScamClosure::isProcedure() const
{
    return true;
}

ExprHandle ScamClosure::withEnvUpdate(Env * updated) const
{
    return ExpressionFactory::makeClosure(parser, updated);
}
