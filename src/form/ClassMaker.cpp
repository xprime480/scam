#include "form/ClassMaker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/Validations.hpp"

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
    const auto len = args->length();

    if ( len < 2 ) {
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

    for ( size_t nth = 2 ; nth < len ; ++nth ) {
        ScamExpr * funcdef = args->nthcar(nth);
        if ( ! funcdef->isCons() ) {
            ScamExpr * err =
                ExpressionFactory::makeError("Expected function def: ",
                                             "(function args forms...)",
                                             "; got ",
                                             funcdef->toString());
            cont->run(err);
            return false;
        }

        ScamExpr * name = funcdef->getCar();
        ScamExpr * funcArgs = funcdef->getCdr();
        ScamExpr * funcOk = validateClosureArgs(funcArgs,
                                                name->toString().c_str());
        if ( funcOk->error() ) {
            cont->run(funcOk);
            return false;
        }
    }

    return true;
}
