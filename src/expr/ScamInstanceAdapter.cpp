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
    return INSTANCEPRIVENV(instance->data);
}

Env * ScamInstanceAdapter::getEnv() const
{
    return INSTANCELOCALENV(instance->data);
}

ExprHandle ScamInstanceAdapter::getParent() const
{
    static ScamEnvKeyType parent = ScamInstance::parent;

    if ( INSTANCELOCALENV(instance->data)->check(parent) ) {
        return INSTANCELOCALENV(instance->data)->get(parent);
    }

    return ExpressionFactory::makeNil();
}
