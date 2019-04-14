
#include "prim/Instantiator.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <sstream>

using namespace std;
using namespace scam;

Instantiator::Instantiator(size_t & counter)
    : counter(counter)
    , dict(ExpressionFactory::makeDict())
{
}

ScamExpr * Instantiator::exec(ScamExpr * args)
{
    bool ok;
    ScamExpr * expr = checkargs(args, ok);
    if ( ! ok ) {
        return expr;
    }

    return inst_value(expr);
}

ScamExpr * Instantiator::map_value(ScamExpr * val)
{
    return inst_value(val);
}

ScamExpr * Instantiator::make_error(ScamExpr * args)
{
    return ExpressionFactory::makeError("Substitute expected one arg got: ",
                                        args->toString());
}

ScamExpr * Instantiator::checkargs(ScamExpr * args, bool & ok)
{
    if ( args->length() < 1u ) {
        ok = false;
        return make_error(args);
    }

    ScamExpr * expr = args->nthcar(0);
    ok = true;
    return expr;
}

ScamExpr * Instantiator::inst_value(ScamExpr * expr)
{
    if ( expr->isKeyword() ) {
        return inst_keyword(expr);
    }
    else if ( expr->isCons() ) {
        return inst_cons(expr);
    }
    else if ( expr->isVector() ) {
        return inst_vector(expr);
    }
    else if ( expr->isDict() ) {
        return inst_dict(expr);
    }
    else {
        return expr;
    }
}

ScamExpr * Instantiator::new_mapping(ScamExpr * expr)
{
    stringstream s;
    s << ":kw" << ++counter;
    ScamExpr * value = ExpressionFactory::makeKeyword(s.str());
    dict->put(expr, value);
    return value;
}

ScamExpr * Instantiator::inst_keyword(ScamExpr * expr)
{
    if ( dict->has(expr) ) {
        return dict->get(expr);
    }

    return new_mapping(expr);
}

ScamExpr * Instantiator::inst_cons(ScamExpr * expr)
{
    return map_cons(expr);
}

ScamExpr * Instantiator::inst_vector(ScamExpr * expr)
{
    return map_vector(expr);
}

ScamExpr * Instantiator::inst_dict(ScamExpr * expr)
{
    return map_dict(expr);
}
