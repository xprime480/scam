#include "prim/MatchUnifyCommon.hpp"

#include "Continuation.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "input/MatchUnifyParser.hpp"
#include "prim/CommonError.hpp"
#include "prim/Substitutor.hpp"

#include <sstream>
#include <string>

using namespace scam;
using namespace std;

MatchUnifyCommon::MatchUnifyCommon(MatchUnifyParser * parser,
                                   Continuation * cont)
    : parser(parser)
    , cont(cont)
{
}

void MatchUnifyCommon::solve()
{
    ScamValue lhs  = parser->getLhs();
    ScamValue rhs  = parser->getRhs();
    ScamValue dict = parser->getDict();

    ScamValue result = exec(dict, lhs, rhs);
    if ( ! parser->isMatch() && isDict(result) ) {
        result = resolve(result);
    }

    cont->run(result);
}

ScamValue
MatchUnifyCommon::check_ignore(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    static ScamValue ignore = makeKeyword("::", false);

    if ( equals(ignore, lhs) || equals(ignore, rhs) ) {
        return dict;
    }

    return makeNil();
}

ScamValue
MatchUnifyCommon::check_literals(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    if ( equals(lhs, rhs) ) {
        return dict;
    }

    return makeNil();
}

ScamValue
MatchUnifyCommon::check_keyword(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    if ( isKeyword(lhs) && ! isNil(rhs) ) {
        ScamValue old = dictGet(dict, lhs);
        if ( error(old) ) {
            dictPut(dict, lhs, rhs);
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

    return makeNil();
}

ScamValue MatchUnifyCommon::check_keyword_reversed(ScamValue dict,
                                                   ScamValue lhs,
                                                   ScamValue rhs)
{
    if ( parser->isMatch() ) {
        return makeNil();
    }
    else {
        return check_keyword(dict, rhs, lhs);
    }
}

ScamValue
MatchUnifyCommon::check_cons(ScamValue dict, ScamValue lhs, ScamValue rhs)
{
    if ( isCons(lhs) && isCons(rhs) ) {
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

    return makeNil();
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

    return makeNil();
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

        KeyVec const & keys = getDictKeys(lhs);
        for ( auto key : keys ) {
            ScamValue d = dictGet(rhs, key);
            if ( error(d) ) {
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

    return makeNil();
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
         &MatchUnifyCommon::check_cons,
         &MatchUnifyCommon::check_vector,
         &MatchUnifyCommon::check_dict
    };

    for ( auto checker : checkers ) {
        ScamValue rv = (this->*checker)(dict, lhs, rhs);
        if ( ! isNil(rv) ) {
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
