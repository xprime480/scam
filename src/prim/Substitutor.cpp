#include "prim/Substitutor.hpp"

#include "expr/ScamExpr.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"
#include "prim/CommonError.hpp"

#include <sstream>

using namespace std;
using namespace scam;

Substitutor::Substitutor(ScamDict * answers)
    : answers(answers)
    , helper(ExpressionFactory::makeNil())
{
}

ExprHandle Substitutor::resolve_value(ExprHandle expr)
{
    ExprHandle rv;

    if ( TypePredicates::isCons(expr) ) {
        rv = resolve_cons(expr);
    }
    else if ( TypePredicates::isVector(expr) ) {
        rv = resolve_vector(expr);
    }
    else if ( TypePredicates::isDict(expr) ) {
        rv = resolve_dict(expr);
    }
    else if ( TypePredicates::isKeyword(expr) ) {
        rv = resolve_keyword(expr);
    }
    else {
        rv = expr;
    }

    return rv;
}

ExprHandle Substitutor::map_value(ExprHandle val)
{
    return resolve_value(val);
}

ExprHandle Substitutor::resolve_cons(ExprHandle expr)
{
    return map_cons(expr);
}

ExprHandle Substitutor::resolve_vector(ExprHandle expr)
{
    return map_vector(expr);
}

bool Substitutor::have_seen(ExprHandle expr)
{
    ExprHandle t = helper;
    while ( ! TypePredicates::isNil(t) ) {
        if ( t->nthcar(0)->equals(expr) ) {
            return true;
        }
        t = t->nthcdr(0);
    }

    return false;
}

ExprHandle Substitutor::resolve_keyword(ExprHandle expr)
{
    if ( have_seen(expr) ) {
        stringstream s;
        s << "Infinite Loop resolving keyword " << ExprWriter::write(expr);
        return make_common_error(s.str().c_str());
    }

    helper = ExpressionFactory::makeCons(expr, helper);

    ExprHandle val = answers->get(expr);
    ExprHandle rv  = resolve_value(val);

    helper = helper->nthcdr(0);
    return rv;
}

ExprHandle Substitutor::resolve_dict(ExprHandle expr)
{
    return map_dict(expr);
}
