#include "expr/ScamContinuation.hpp"

#include "Continuation.hpp"
#include "input/SingletonParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

#define CONTINUATION(data) ((data).value.contData)

ScamContinuation::ScamContinuation(Continuation * cont)
    : ScamExpr(ScamData::Cont)
{
    CONTINUATION(data) = cont;
}

ScamContinuation * ScamContinuation::makeInstance(Continuation * cont)
{
    return new ScamContinuation(cont);
}

void ScamContinuation::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        CONTINUATION(data)->mark();
    }
}

string ScamContinuation::toString() const
{
    static const string value { "continuation" };
    return value;
}

void ScamContinuation::apply(ExprHandle args, Continuation * cont,  Env * env)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
    const bool accepted = parser->accept(args);

    if ( accepted ) {
        ExprHandle arg = const_cast<ExprHandle >(parser->get());
        CONTINUATION(data)->run(arg);
    }
    else {
        failedArgParseMessage(toString().c_str(), "(form)", args, cont);
    }
}
