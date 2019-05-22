#include "expr/ScamInstanceAdapter.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamInstance.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamInstanceAdapter::ScamInstanceAdapter(ExprHandle expr)
    : instance(dynamic_cast<ScamInstance const *>(expr))
{
    if ( ! expr->isInstance() ) {
        stringstream s;
        s << "ScamInstanceAdapter expected an instance, got: "
          << expr->toString();
        throw ScamException(s.str());
    }
}

Env * ScamInstanceAdapter::getFunctionMap() const
{
    return INSTANCEPRIVENV(instance);
}

Env * ScamInstanceAdapter::getEnv() const
{
    return INSTANCELOCALENV(instance);
}

ExprHandle ScamInstanceAdapter::getParent() const
{
    static ScamEnvKeyType parent = ScamInstance::parent;

    if ( INSTANCELOCALENV(instance)->check(parent) ) {
        return INSTANCELOCALENV(instance)->get(parent);
    }

    return ExpressionFactory::makeNil();
}
