
#include "prim/Add.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Add::Add()
    : Primitive("Add")
{
}

void
Add::applyArgs(shared_ptr<ScamExpr> const & args, shared_ptr<Continuation> cont)
{
    if ( ! args->isList() ) {
        stringstream s;
        s << toString() << " expects list of numeric, got " << args->toString();
        shared_ptr<ScamExpr> err = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return;
    }

    double sum { 0 };
    bool floaty { false };

    const size_t len = args->length();
    for ( size_t idx = 0u ; idx < len ; ++idx ) {
        shared_ptr<ScamExpr> const arg = args->nth(idx);
        if ( ! arg->isNumeric() ) {
            stringstream s;
            s << toString() << " expects numeric, got " << arg->toString();
            shared_ptr<ScamExpr> err = ExpressionFactory::makeError(s.str());
            cont->run(err);
            return;
        }

        if ( ! arg->isInteger() ) {
            floaty = true;
        }
        sum += arg->toFloat();
    }

    shared_ptr<ScamExpr> rv;
    if ( floaty ) {
        rv = ExpressionFactory::makeFloat((int)sum);
    }
    else {
        rv = ExpressionFactory::makeInteger((int)sum);
    }
    cont->run(rv);
}

shared_ptr<ScamExpr> Add::clone()
{
    return ExpressionFactory::makeForm<Add>();
}
