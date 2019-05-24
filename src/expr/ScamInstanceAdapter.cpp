#include "expr/ScamInstanceAdapter.hpp"

#include "Env.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamInstance.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamInstanceAdapter::ScamInstanceAdapter(ScamValue expr)
    : instance(dynamic_cast<ScamInstance const *>(expr))
{
    if ( ! isInstance(expr) ) {
        stringstream s;
        s << "ScamInstanceAdapter expected an instance, got: "
          << writeValue(expr);
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

ScamValue ScamInstanceAdapter::getParent() const
{
    static ScamEnvKeyType parent = ScamInstance::parent;

    if ( INSTANCELOCALENV(instance)->check(parent) ) {
        return INSTANCELOCALENV(instance)->get(parent);
    }

    return ExpressionFactory::makeNil();
}
