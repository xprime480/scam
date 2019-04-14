
#include "form/ClassMaker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ClassMaker::ClassMaker()
    : SpecialForm("class-maker")
{
}

ClassMaker * ClassMaker::makeInstance()
{
    static ClassMaker instance;
    return &instance;
}

void ClassMaker::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    if ( ! validate_args(args, cont) ) {
        return;
    }

    ScamExpr * base = args->nthcar(0);
    ScamExpr * parms = args->nthcar(1);
    ScamExpr * funcs = args->nthcdr(1);

    ScamExpr * cls =
        ExpressionFactory::makeClass(base, parms, funcs, env);
    cont->run(cls);
}

bool ClassMaker::validate_args(ScamExpr * args, Continuation * cont)
{
    if ( args->length() < 2 ) {
        ScamExpr * err =
            ExpressionFactory::makeError("Expected: (make-class Base",
                                         " (vars...) methods...); ",
                                         "got ",
                                         args->toString());
        cont->run(err);
        return false;
    }

    ScamExpr * vars = args->nthcar(1);
    if ( ! vars->isList() ) {
        ScamExpr * err =
            ExpressionFactory::makeError("Expected list of vars, got ",
                                         vars->toString());
        cont->run(err);
        return false;
    }

    return true;
}
