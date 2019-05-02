#include "expr/ScamContinuation.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/SingletonParser.hpp"

using namespace scam;
using namespace std;

ScamContinuation::ScamContinuation(Continuation * cont)
    : cont(cont)
{
}

ScamContinuation * ScamContinuation::makeInstance(Continuation * cont)
{
    return new ScamContinuation(cont);
}

void ScamContinuation::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        cont->mark();
    }
}

string ScamContinuation::toString() const
{
    static const string value { "continuation" };
    return value;
}

bool ScamContinuation::hasApply() const
{
    return true;
}

void
ScamContinuation::apply(ExprHandle args, Continuation * cont,  Env * env)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
    const bool accepted = parser->accept(args);

    if ( accepted ) {
        ExprHandle arg = const_cast<ExprHandle >(parser->get());
        this->cont->run(arg);
    }
    else {
        ExprHandle err =
            ExpressionFactory::makeError("Continuation expects to be ",
                                         "applied to 1 arg; received ",
                                         args->toString());
        cont->run(err);
    }
}
