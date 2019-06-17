#include "expr/DictOps.hpp"

#include "ScamException.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void checkDict(ScamValue value, const char * op);
}

bool scam::dictHas(ScamValue value, ScamValue key)
{
    checkDict(value, "dictHas");
    const ScamData::DictKeyData & keys = value->dictKeys();

    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( equals(keys[jdx], key) ) {
            return true;
        }
    }

    return false;
}

ScamValue scam::dictGet(ScamValue value, ScamValue key)
{
    checkDict(value, "dictGet");
    const ScamData::DictKeyData & keys = value->dictKeys();

    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( equals(keys[jdx], key) ) {
            return (value->dictValues())[jdx];
        }
    }

    return makeErrorExtended("Dict key '", writeValue(key), "' does not exist");
}

ScamValue scam::dictPut(ScamValue value, ScamValue key, ScamValue val)
{
    checkDict(value, "dictPut");

    ScamData::DictKeyData   & keys = value->dictKeys();
    ScamData::DictValueData & vals = value->dictValues();
    size_t prev = keys.size();

    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( equals(keys[jdx], key) ) {
            prev = jdx;
            break;
        }
    }

    if ( prev >= keys.size() ) {
        keys.push_back(key);
        vals.push_back(val);
    }
    else {
        vals[prev] = val;
    }

    return val;
}

ScamValue scam::dictRemove(ScamValue value, ScamValue key)
{
    checkDict(value, "dictRemove");

    ScamData::DictKeyData   & keys = value->dictKeys();
    ScamData::DictValueData & vals = value->dictValues();
    ScamValue rv = makeNull();

    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( equals(keys[jdx], key) ) {
            keys.erase(keys.begin() + jdx);
            rv = vals[jdx];
            vals.erase(vals.begin() + jdx);
            break;
        }
    }

    return rv;
}

const KeyVec & scam::getDictKeys(ScamValue value)
{
    checkDict(value, "getDictKeys");

    return value->dictKeys();
}

namespace
{
    void checkDict(ScamValue value, const char * op)
    {
        if ( ! isDict(value) ) {
            stringstream s;
            s << "cannot perform dictionary operation " << op
              << "on non-dict value " << writeValue(value);
            throw ScamException(s.str());
        }
    }
}
