#include "expr/ScamDict.hpp"

#include "Continuation.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamDict::ScamDict()
    : ScamExpr(ScamData::Dict)
{
}

ScamDict::ScamDict(ValVec const & args)
    : ScamDict()
{
    ValVec input = args;
    if ( 1 == (input.size() % 2) ) {
        input.push_back(ExpressionFactory::makeNil());
    }

    for ( size_t idx = 0 ; idx < input.size() ; idx += 2 ) {
        ScamValue key = input[idx];
        ScamValue val = input[idx+1];
        put(key, val);
    }
}

ScamDict * ScamDict::makeInstance()
{
    return new ScamDict();
}

ScamDict * ScamDict::makeInstance(ValVec const & args)
{
    return new ScamDict(args);
}

bool ScamDict::has(ScamValue key) const
{
    for ( size_t jdx = 0 ; jdx < DICTKEYS(this).size() ; ++jdx ) {
        if ( equals(DICTKEYS(this)[jdx], key) ) {
            return true;
        }
    }

    return false;
}

ScamValue ScamDict::get(ScamValue key) const
{
    for ( size_t jdx = 0 ; jdx < DICTKEYS(this).size() ; ++jdx ) {
        if ( equals(DICTKEYS(this)[jdx], key) ) {
            return DICTVALS(this)[jdx];
        }
    }

    return ExpressionFactory::makeError("Dict key '",
                                        writeValue(key),
                                        "' does not exist");
}

ScamValue ScamDict::put(ScamValue key, ScamValue val)
{
    size_t prev = DICTKEYS(this).size();

    for ( size_t jdx = 0 ; jdx < DICTKEYS(this).size() ; ++jdx ) {
        if ( equals(DICTKEYS(this)[jdx], key) ) {
            prev = jdx;
            break;
        }
    }

    if ( prev >= DICTKEYS(this).size() ) {
        DICTKEYS(this).push_back(key);
        DICTVALS(this).push_back(val);
    }
    else {
        DICTVALS(this)[prev] = val;
    }

    return val;
}

ScamValue ScamDict::remove(ScamValue key)
{
    ScamValue rv = ExpressionFactory::makeNil();

    for ( size_t jdx = 0 ; jdx < DICTKEYS(this).size() ; ++jdx ) {
        if ( equals(DICTKEYS(this)[jdx], key) ) {
            DICTKEYS(this).erase(DICTKEYS(this).begin() + jdx);
            rv = DICTVALS(this)[jdx];
            DICTVALS(this).erase(DICTVALS(this).begin() + jdx);
            break;
        }
    }

    return rv;
}

KeyVec const & ScamDict::getKeys() const
{
    return DICTKEYS(this);
}
