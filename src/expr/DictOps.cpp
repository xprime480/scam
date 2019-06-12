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

    for ( size_t jdx = 0 ; jdx < DICTKEYS(value).size() ; ++jdx ) {
        if ( equals(DICTKEYS(value)[jdx], key) ) {
            return true;
        }
    }

    return false;
}

ScamValue scam::dictGet(ScamValue value, ScamValue key)
{
    checkDict(value, "dictGet");

    for ( size_t jdx = 0 ; jdx < DICTKEYS(value).size() ; ++jdx ) {
        if ( equals(DICTKEYS(value)[jdx], key) ) {
            return DICTVALS(value)[jdx];
        }
    }

    return makeErrorExtended("Dict key '", writeValue(key), "' does not exist");
}

ScamValue scam::dictPut(ScamValue value, ScamValue key, ScamValue val)
{
    checkDict(value, "dictPut");

    size_t prev = DICTKEYS(value).size();

    for ( size_t jdx = 0 ; jdx < DICTKEYS(value).size() ; ++jdx ) {
        if ( equals(DICTKEYS(value)[jdx], key) ) {
            prev = jdx;
            break;
        }
    }

    if ( prev >= DICTKEYS(value).size() ) {
        DICTKEYS(value).push_back(key);
        DICTVALS(value).push_back(val);
    }
    else {
        DICTVALS(value)[prev] = val;
    }

    return val;
}

ScamValue scam::dictRemove(ScamValue value, ScamValue key)
{
    checkDict(value, "dictRemove");

    ScamValue rv = makeNull();

    for ( size_t jdx = 0 ; jdx < DICTKEYS(value).size() ; ++jdx ) {
        if ( equals(DICTKEYS(value)[jdx], key) ) {
            DICTKEYS(value).erase(DICTKEYS(value).begin() + jdx);
            rv = DICTVALS(value)[jdx];
            DICTVALS(value).erase(DICTVALS(value).begin() + jdx);
            break;
        }
    }

    return rv;
}

const KeyVec & scam::getDictKeys(ScamValue value)
{
    checkDict(value, "getDictKeys");

    return DICTKEYS(value);
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
