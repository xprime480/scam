
#include "prim/Add.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Add::Add()
    : Primitive("Add")
{
}

void
Add::applyArgs(shared_ptr<ScamExpr> const & args, shared_ptr<Continuation> cont)
{
    shared_ptr<ScamExpr> rv = ExpressionFactory::makeInteger(4);
    cont->run(rv);
}

shared_ptr<ScamExpr> Add::clone()
{
    return ExpressionFactory::makeForm<Add>();
}
