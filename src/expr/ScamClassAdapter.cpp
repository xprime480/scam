
#include "expr/ScamClassAdapter.hpp"

#include "ScamException.hpp"
#include "expr/ScamClass.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamClassAdapter::ScamClassAdapter(ScamExpr const * expr)
    : cls(dynamic_cast<ScamClass const *>(expr))
{
    if ( ! expr->isClass() ) {
        stringstream s;
        s << "ScamClassAdapter expected a class, got: " << expr->toString();
        throw ScamException(s.str());
    }
}

ScamExpr * ScamClassAdapter::getBase() const
{
    return cls->base.get();
}

ScamExpr * ScamClassAdapter::getVars() const
{
    return cls->vars.get();
}

ScamExpr * ScamClassAdapter::getFuns() const
{
    return cls->funs.get();
}

Env ScamClassAdapter::getCapture() const
{
    return cls->capture;
}