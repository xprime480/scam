#include "expr/ScamClassAdapter.hpp"

#include "ScamException.hpp"
#include "expr/ScamClass.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamClassAdapter::ScamClassAdapter(ConstExprHandle expr)
    : cls(dynamic_cast<ScamClass const *>(expr))
{
    if ( ! expr->isClass() ) {
        stringstream s;
        s << "ScamClassAdapter expected a class, got: " << expr->toString();
        throw ScamException(s.str());
    }
}

ExprHandle ScamClassAdapter::getBase() const
{
    return cls->base;
}

ExprHandle ScamClassAdapter::getVars() const
{
    return cls->vars;
}

ExprHandle ScamClassAdapter::getFuns() const
{
    return cls->funs;
}

Env * ScamClassAdapter::getCapture() const
{
    return cls->capture;
}
