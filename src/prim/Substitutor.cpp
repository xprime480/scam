#include "prim/Substitutor.hpp"

#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "prim/CommonError.hpp"

#include <sstream>

using namespace std;
using namespace scam;

Substitutor::Substitutor(ScamDict * answers)
    : answers(answers)
    , helper(ExpressionFactory::makeNil())
{
}

ScamExpr * Substitutor::resolve_value(ScamExpr * expr)
{
    ScamExpr * rv;

    if ( expr->isCons() ) {
        rv = resolve_cons(expr);
    }
    else if ( expr->isVector() ) {
        rv = resolve_vector(expr);
    }
    else if ( expr->isDict() ) {
        rv = resolve_dict(expr);
    }
    else if ( expr->isKeyword() ) {
        rv = resolve_keyword(expr);
    }
    else {
        rv = expr;
    }

    return rv;
}

ScamExpr * Substitutor::map_value(ScamExpr * val)
{
    return resolve_value(val);
}

ScamExpr * Substitutor::resolve_cons(ScamExpr * expr)
{
    return map_cons(expr);
}

ScamExpr * Substitutor::resolve_vector(ScamExpr * expr)
{
    return map_vector(expr);
}

bool Substitutor::have_seen(ScamExpr * expr)
{
    ScamExpr * t = helper;
    while ( ! t->isNil() ) {
        if ( t->nthcar(0)->equals(expr) ) {
            return true;
        }
        t = t->nthcdr(0);
    }

    return false;
}

ScamExpr * Substitutor::resolve_keyword(ScamExpr * expr)
{
    if ( have_seen(expr) ) {
        stringstream s;
        s << "Infinite Loop resolving keyword " << expr->toString();
        return make_common_error(s.str().c_str());
    }

    helper = ExpressionFactory::makeCons(expr, helper);

    ScamExpr * val = answers->get(expr);
    ScamExpr * rv  = resolve_value(val);

    helper = helper->nthcdr(0);
    return rv;
}

ScamExpr * Substitutor::resolve_dict(ScamExpr * expr)
{
    return map_dict(expr);
}
