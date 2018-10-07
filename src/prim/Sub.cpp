
#include "prim/Sub.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Sub::Sub()
    : Primitive("Sub")
{
}

void Sub::applyArgs(ExprHandle const & args, ContHandle cont)
{
    if ( ! args->isList() ) {
        stringstream s;
        s << toString() << " expects list of numeric, got " << args->toString();
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return;
    }

    double total { 0 };
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
        if ( len > 1u && 0 == idx ) {
            total = arg->toFloat();
        }
        else {
            total -=  arg->toFloat();
        }
    }

    ExprHandle rv;
    if ( floaty ) {
        rv = ExpressionFactory::makeFloat((int)total);
    }
    else {
        rv = ExpressionFactory::makeInteger((int)total);
    }
    cont->run(rv);
}

ExprHandle Sub::clone()
{
    return ExpressionFactory::makeForm<Sub>();
}
