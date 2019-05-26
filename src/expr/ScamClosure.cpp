#include "expr/ScamClosure.hpp"

using namespace scam;
using namespace std;

ScamClosure::ScamClosure(const LambdaParser * parser, Env * env, bool macrolike)
    : ScamData(ScamData::Closure)
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
