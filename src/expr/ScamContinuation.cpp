#include "expr/ScamContinuation.hpp"

#include "Continuation.hpp"
#include "input/SingletonParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

ScamContinuation::ScamContinuation(Continuation * cont)
    : ScamExpr(ScamData::Cont)
{
    CONTINUATION(this) = cont;
}

ScamContinuation * ScamContinuation::makeInstance(Continuation * cont)
{
    return new ScamContinuation(cont);
}

void ScamContinuation::apply(ExprHandle args, Continuation * cont,  Env * env)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
    const bool accepted = parser->accept(args);

    if ( accepted ) {
        ExprHandle arg = const_cast<ExprHandle >(parser->get());
        CONTINUATION(this)->run(arg);
    }
    else {
        failedArgParseMessage(toString().c_str(), "(form)", args, cont);
    }
}
