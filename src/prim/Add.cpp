
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

void Add::applyArgs(ExprHandle const & args, ContHandle cont)
{
    if ( ! args->isList() ) {
        stringstream s;
        s << toString() << " expects list of numeric, got " << args->toString();
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return;
    }

    double sum { 0 };
    bool floaty { false };

    const size_t len = args->length();
    for ( size_t idx = 0u ; idx < len ; ++idx ) {
        ExprHandle const arg = args->nth(idx);
        if ( ! arg->isNumeric() ) {
            stringstream s;
            s << toString() << " expects numeric, got " << arg->toString();
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err);
            return;
        }

        if ( ! arg->isInteger() ) {
            floaty = true;
        }
        sum += arg->toFloat();
    }

    ExprHandle rv;
    if ( floaty ) {
        rv = ExpressionFactory::makeFloat((int)sum);
    }
    else {
        rv = ExpressionFactory::makeInteger((int)sum);
    }
    cont->run(rv);
}

ExprHandle Add::clone()
{
    return ExpressionFactory::makeForm<Add>();
}
