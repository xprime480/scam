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

void ScamContinuation::apply(ScamValue args, Continuation * cont,  Env * env)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
    const bool accepted = parser->accept(args);

    if ( accepted ) {
        ScamValue arg = const_cast<ScamValue >(parser->get());
        CONTINUATION(this)->run(arg);
    }
    else {
        failedArgParseMessage(ExprWriter::write(this).c_str(), "(form)",
                              args,
                              cont);
    }
}
