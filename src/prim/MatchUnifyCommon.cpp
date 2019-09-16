#include "prim/MatchUnifyCommon.hpp"

#include "Continuation.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/SequenceOps.hpp"
#include "prim/CommonError.hpp"
#include "prim/Substitutor.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

#include <sstream>
#include <string>

using namespace scam;
using namespace std;

MatchUnifyCommon::MatchUnifyCommon(ScamValue lhs,
                                   ScamValue rhs,
                                   ScamValue dict,
                                   bool match,
                                   Continuation * cont)
    : lhs(lhs)
    , rhs(rhs)
    , dict(dict)
    , match(match)
    , cont(cont)
{
}

void MatchUnifyCommon::solve()
{
    ScamValue result = exec(dict, lhs, rhs);
    if ( ! match && isDict(result) ) {
        result = resolve(result);
    }

    cont->handleValue(result);
}

ScamValue
MatchUnifyCommon::check_ignore(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    static ScamValue ignore = makeKeyword("::", false);

    if ( equals(ignore, lhs) || equals(ignore, rhs) ) {
        return dict;
    }

    return makeNull();
}

ScamValue
MatchUnifyCommon::check_literals(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    if ( equals(lhs, rhs) ) {
        return dict;
    }

    return makeNull();
}

ScamValue
MatchUnifyCommon::check_keyword(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    if ( isKeyword(lhs) && ! isNull(rhs) ) {
        ScamValue old = dictGet(dict, lhs);
        if ( isError(old) ) {
            ScamValue test = dictPut(dict, lhs, rhs);
            if ( isError(test) ) {
                return test;
            }
        }
        else if ( ! equals(old, rhs) ) {
            stringstream s;
            s << "Previous data for pattern " << writeValue(lhs)
              << " was '" << writeValue(old)
              << "'; does not match new data '"
              << writeValue(rhs) << "'";
            return make_common_error(s.str().c_str());
        }

        return dict;
    }

    return makeNull();
}

ScamValue MatchUnifyCommon::check_keyword_reversed(ScamValue dict,
                                                   ScamValue lhs,
                                                   ScamValue rhs)
{
    if ( match ) {
        return makeNull();
    }
    else {
        return check_keyword(dict, rhs, lhs);
    }
}

ScamValue
MatchUnifyCommon::check_pair(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    if ( isPair(lhs) && isPair(rhs) ) {
        ScamValue lhsCar  = getCar(lhs);
        ScamValue rhsCar  = getCar(rhs);
        ScamValue carMatch = exec(dict, lhsCar, rhsCar);
        if ( ! isDict(carMatch) ) {
            return carMatch;
        }

        ScamValue lhsCdr  = getCdr(lhs);
        ScamValue rhsCdr  = getCdr(rhs);
        return exec(dict, lhsCdr, rhsCdr);
    }

    return makeNull();
}

ScamValue
MatchUnifyCommon::check_vector(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    if ( isVector(lhs) && isVector(rhs) ) {
        if ( length(lhs) != length(rhs) ) {
            stringstream s;
            s << "matching vectors of unequal length: "
              << writeValue(lhs) << " and " << writeValue(rhs);
            return make_common_error(s.str().c_str());
        }

        size_t len = length(lhs);
        for ( size_t idx = 0 ; idx < len ; ++ idx ) {
            ScamValue lhsN = nthcar(lhs, idx);
            ScamValue rhsN = nthcar(rhs, idx);
            ScamValue matchN = exec(dict, lhsN, rhsN);
            if ( ! isDict(matchN) ) {
                return matchN;
            }
        }

        return dict;
    }

    return makeNull();
}

ScamValue
MatchUnifyCommon::check_dict(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    if ( isDict(lhs) && isDict(rhs) ) {
        if ( length(lhs) != length(rhs) ) {
            stringstream s;
            s << "dictionaries do not match " << writeValue(lhs)
              << "; " << writeValue(rhs);
            return make_common_error(s.str().c_str());
        }

        vector<ScamValue> const * pKeys { nullptr };
        ScamValue test = getDictKeys(lhs, pKeys);
        if ( isError(test) ) {
            return test;
        }
        vector<ScamValue> const & keys = *pKeys;

        for ( auto key : keys ) {
            ScamValue d = dictGet(rhs, key);
            if ( isError(d) ) {
                return make_common_error(writeValue(d).c_str());
            }
            ScamValue p = dictGet(lhs, key);
            ScamValue result = exec(dict, p, d);
            if ( ! isDict(result) ) {
                return result;
            }
        }

        return dict;
    }

    return makeNull();
}

ScamValue MatchUnifyCommon::exec(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    using CheckerType =
        ScamValue (MatchUnifyCommon::*)(ScamValue, ScamValue, ScamValue);

    static CheckerType checkers[] = {
         &MatchUnifyCommon::check_ignore,
         &MatchUnifyCommon::check_literals,
         &MatchUnifyCommon::check_keyword,
         &MatchUnifyCommon::check_keyword_reversed,
         &MatchUnifyCommon::check_pair,
         &MatchUnifyCommon::check_vector,
         &MatchUnifyCommon::check_dict
    };

    for ( auto checker : checkers ) {
        ScamValue rv = (this->*checker)(dict, lhs, rhs);
        if ( ! isNull(rv) ) {
          return rv;
        }
    }

    stringstream s;
    s << "Pattern: " << writeValue(lhs)
      << " does not conform to data: " << writeValue(rhs);
    return make_common_error(s.str().c_str());
}

ScamValue MatchUnifyCommon::resolve(ScamValue expr)
{
    Substitutor subst(expr);
    return subst.resolve_value(expr);
}
