#include "expr/DictOps.hpp"

#include "ErrorCategory.hpp"
#include "expr/EqualityOps.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern ScamValue checkDict(ScamValue value, const char * op);
}

ScamValue scam::dictHas(ScamValue value, ScamValue key)
{
    ScamValue test = checkDict(value, "dictHas");
    if ( isError(test) ) {
        return test;
    }

    const vector<ScamValue> & keys = value->dictKeys();

    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( equals(keys[jdx], key) ) {
            return makeBoolean(true);
        }
    }

    return makeBoolean(false);
}

ScamValue scam::dictGet(ScamValue value, ScamValue key)
{
    ScamValue test = checkDict(value, "dictGet");
    if ( isError(test) ) {
        return test;
    }

    const vector<ScamValue> & keys = value->dictKeys();

    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( equals(keys[jdx], key) ) {
            return (value->dictValues())[jdx];
        }
    }

    ScamValue rv = makeError("Key not found (%{0})", key);
    rv->errorCategory() = dictCategory;
    return rv;
}

ScamValue scam::dictPut(ScamValue value, ScamValue key, ScamValue val)
{
    ScamValue test = checkDict(value, "dictPut");
    if ( isError(test) ) {
        return test;
    }

    vector<ScamValue>   & keys = value->dictKeys();
    vector<ScamValue> & vals = value->dictValues();
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
    ScamValue test = checkDict(value, "dictRemove");
    if ( isError(test) ) {
        return test;
    }

    vector<ScamValue>   & keys = value->dictKeys();
    vector<ScamValue> & vals = value->dictValues();
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

ScamValue scam::getDictKeys(ScamValue value, const vector<ScamValue> *& result)
{
    ScamValue test = checkDict(value, "getDictKeys");
    if ( isError(test) ) {
        return test;
    }

    result = &value->dictKeys();
    return makeNothing();
}

namespace
{
    ScamValue checkDict(ScamValue value, const char * op)
    {
        if ( ! isDict(value) ) {
            static const char * msg
            { "cannot perform dictionary operation %{0} "
                 "on non-dict value %{1}" };

            ScamValue err = makeError(msg , makeString(op), value);
            err->errorCategory() = dictCategory;
            return err;
        }

        return makeNothing();
    }
}
