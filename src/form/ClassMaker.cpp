
#include "form/ClassMaker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void do_apply(ScamExpr * args, ContHandle cont, Env env);
}

ClassMaker::ClassMaker()
    : SpecialForm("class-maker")
{
}

void ClassMaker::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(args, cont, env);
}

namespace
{
    bool validate_args(ScamExpr * args, ContHandle cont)
    {
        if ( args->length() < 2 ) {
            stringstream s;
            s << "Expected: (make-class Base (vars...) methods...); ";
            s << "got " << args->toString();
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
            return false;
        }

        ExprHandle vars = args->nthcar(1);
        if ( ! vars->isList() ) {
            stringstream s;
            s << "Expected list of vars, got " << vars->toString();
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
            return false;
        }

        return true;
    }

    void do_apply(ScamExpr * args, ContHandle cont, Env env)
    {
        if ( ! validate_args(args, cont) ) {
            return;
        }

        ExprHandle base = args->nthcar(0);
        ExprHandle parms = args->nthcar(1);
        ExprHandle funcs = args->nthcdr(1);

        ExprHandle cls =
            ExpressionFactory::makeClass(base.get(), parms.get(), funcs.get());
        cont->run(cls.get());
    }
}
