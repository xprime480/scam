#include "form/Eval.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "form/EvalCont.hpp"
#include "input/SingletonParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

Eval::Eval()
    : SpecialForm("eval")
{
}

Eval * Eval::makeInstance()
{
    static Eval instance;
    return &instance;
}

void Eval::apply(ExprHandle args, Continuation * cont, Env * env)
{
    SingletonParser * parser = getSingletonOfAnythingParser();

    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("Eval expects single form, got: ",
                                         args->toString());
        cont->run(err);
    }
    else {
        Continuation * finisher =
            standardMemoryManager.make<EvalCont>(cont, env);
        ExprHandle expr = const_cast<ExprHandle>(parser->get());
        expr->eval(finisher, env);
    }
}
