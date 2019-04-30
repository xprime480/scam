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

ExprHandle Instantiator::exec(ExprHandle args)
{
    bool ok;
    ExprHandle expr = checkargs(args, ok);
    if ( ! ok ) {
        return expr;
    }

    return inst_value(expr);
}

ExprHandle Instantiator::map_value(ExprHandle val)
{
    return inst_value(val);
}

ExprHandle Instantiator::make_error(ExprHandle args)
{
    return ExpressionFactory::makeError("Substitute expected one arg got: ",
                                        args->toString());
}

ExprHandle Instantiator::checkargs(ExprHandle args, bool & ok)
{
    if ( args->length() < 1u ) {
        ok = false;
        return make_error(args);
    }

    ExprHandle expr = args->nthcar(0);
    ok = true;
    return expr;
}

ExprHandle Instantiator::inst_value(ExprHandle expr)
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

ExprHandle Instantiator::new_mapping(ExprHandle expr)
{
    stringstream s;
    s << ":kw" << ++counter;
    ExprHandle value = ExpressionFactory::makeKeyword(s.str());
    dict->put(expr, value);
    return value;
}

ExprHandle Instantiator::inst_keyword(ExprHandle expr)
{
    if ( dict->has(expr) ) {
        return dict->get(expr);
    }

    return new_mapping(expr);
}

ExprHandle Instantiator::inst_cons(ExprHandle expr)
{
    return map_cons(expr);
}

ExprHandle Instantiator::inst_vector(ExprHandle expr)
{
    return map_vector(expr);
}

ExprHandle Instantiator::inst_dict(ExprHandle expr)
{
    return map_dict(expr);
}
