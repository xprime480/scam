
#include "form/ClassMaker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void do_apply(ScamExpr * args, Continuation * cont, Env * env);
}

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
    do_apply(args, cont, env);
}

namespace
{
    bool validate_args(ScamExpr * args, Continuation * cont)
    {
        if ( args->length() < 2 ) {
            stringstream s;
            s << "Expected: (make-class Base (vars...) methods...); ";
            s << "got " << args->toString();
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
            return false;
        }

        ScamExpr * vars = args->nthcar(1);
        if ( ! vars->isList() ) {
            stringstream s;
            s << "Expected list of vars, got " << vars->toString();
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
            return false;
        }

        return true;
    }

    void do_apply(ScamExpr * args, Continuation * cont, Env * env)
    {
        if ( ! validate_args(args, cont) ) {
            return;
        }

        ScamExpr * base = args->nthcar(0);
        ScamExpr * parms = args->nthcar(1);
        ScamExpr * funcs = args->nthcdr(1);

        ScamExpr * cls = ExpressionFactory::makeClass(base, parms, funcs, env);
        cont->run(cls);
    }
}
