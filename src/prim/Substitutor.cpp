#include "prim/Substitutor.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "prim/CommonError.hpp"

#include <sstream>

using namespace std;
using namespace scam;

Substitutor::Substitutor(ScamDict * answers)
    : answers(answers)
    , helper(ExpressionFactory::makeNil())
{
}

ScamValue Substitutor::resolve_value(ScamValue expr)
{
    ScamValue rv;

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

ScamValue Substitutor::map_value(ScamValue val)
{
    return resolve_value(val);
}

ScamValue Substitutor::resolve_cons(ScamValue expr)
{
    return map_cons(expr);
}

ScamValue Substitutor::resolve_vector(ScamValue expr)
{
    return map_vector(expr);
}

bool Substitutor::have_seen(ScamValue expr)
{
    ScamValue t = helper;
    while ( ! TypePredicates::isNil(t) ) {
        if ( t->nthcar(0)->equals(expr) ) {
            return true;
        }
        t = t->nthcdr(0);
    }

    return false;
}

ScamValue Substitutor::resolve_keyword(ScamValue expr)
{
    if ( have_seen(expr) ) {
        stringstream s;
        s << "Infinite Loop resolving keyword " << writeValue(expr);
        return make_common_error(s.str().c_str());
    }

    helper = ExpressionFactory::makeCons(expr, helper);

    ScamValue val = answers->get(expr);
    ScamValue rv  = resolve_value(val);

    helper = helper->nthcdr(0);
    return rv;
}

ScamValue Substitutor::resolve_dict(ScamValue expr)
{
    return map_dict(expr);
}
