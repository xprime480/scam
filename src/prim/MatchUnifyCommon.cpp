#include "prim/MatchUnifyCommon.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamDict.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
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
    ScamDict * dict = parser->getDict();

    ScamValue result = exec(dict, lhs, rhs);
    if ( ! parser->isMatch() && isDict(result) ) {
        result = resolve(result);
    }

    cont->run(result);
}

ScamValue MatchUnifyCommon::check_ignore(ScamDict * dict,
                                          ScamValue lhs,
                                          ScamValue rhs)
{
    static ScamValue ignore =
        ExpressionFactory::makeKeyword("::", false);

    if ( ignore->equals(lhs) || ignore->equals(rhs) ) {
        return dict;
    }

    return ExpressionFactory::makeNil();
}

ScamValue MatchUnifyCommon::check_literals(ScamDict * dict,
                                            ScamValue lhs,
                                            ScamValue rhs)
{
    if ( lhs->equals(rhs) ) {
        return dict;
    }

    return ExpressionFactory::makeNil();
}

ScamValue MatchUnifyCommon::check_keyword(ScamDict * dict,
                                           ScamValue lhs,
                                           ScamValue rhs)
{
    if ( isKeyword(lhs) && ! isNil(rhs) ) {
        ScamValue old = dict->get(lhs);
        if ( error(old) ) {
            dict->put(lhs, rhs);
        }
        else if ( ! old->equals(rhs) ) {
            stringstream s;
            s << "Previous data for pattern " << writeValue(lhs)
              << " was '" << writeValue(old)
              << "'; does not match new data '"
              << writeValue(rhs) << "'";
            return make_common_error(s.str().c_str());
        }

        return dict;
    }

    return ExpressionFactory::makeNil();
}

ScamValue MatchUnifyCommon::check_keyword_reversed(ScamDict * dict,
                                                    ScamValue lhs,
                                                    ScamValue rhs)
{
    if ( parser->isMatch() ) {
        return ExpressionFactory::makeNil();
    }
    else {
        return check_keyword(dict, rhs, lhs);
    }
}

ScamValue MatchUnifyCommon::check_cons(ScamDict * dict,
                                        ScamValue lhs,
                                        ScamValue rhs)
{
    if ( isCons(lhs) && isCons(rhs) ) {
        ScamValue lhsCar  = lhs->getCar();
        ScamValue rhsCar  = rhs->getCar();
        ScamValue carMatch = exec(dict, lhsCar, rhsCar);
        if ( ! isDict(carMatch) ) {
            return carMatch;
        }

        ScamValue lhsCdr  = lhs->getCdr();
        ScamValue rhsCdr  = rhs->getCdr();
        return exec(dict, lhsCdr, rhsCdr);
    }

    return ExpressionFactory::makeNil();
}

ScamValue MatchUnifyCommon::check_vector(ScamDict * dict,
                                          ScamValue lhs,
                                          ScamValue rhs)
{
    if ( isVector(lhs) && isVector(rhs) ) {
        if ( lhs->length() != rhs->length() ) {
            stringstream s;
            s << "matching vectors of unequal length: "
              << writeValue(lhs) << " and " << writeValue(rhs);
            return make_common_error(s.str().c_str());
        }

        size_t len = lhs->length();
        for ( size_t idx = 0 ; idx < len ; ++ idx ) {
            ScamValue lhsN = lhs->nthcar(idx);
            ScamValue rhsN = rhs->nthcar(idx);
            ScamValue matchN = exec(dict, lhsN, rhsN);
            if ( ! isDict(matchN) ) {
                return matchN;
            }
        }

        return dict;
    }

    return ExpressionFactory::makeNil();
}

ScamValue MatchUnifyCommon::check_dict(ScamDict * dict,
                                        ScamValue lhs,
                                        ScamValue rhs)
{
    if ( isDict(lhs) && isDict(rhs) ) {
        if ( lhs->length() != rhs->length() ) {
            stringstream s;
            s << "dictionaries do not match " << writeValue(lhs)
              << "; " << writeValue(rhs);
            return make_common_error(s.str().c_str());
        }

        ScamDict * lhsAsDict = dynamic_cast<ScamDict *>(lhs);
        ScamDict * rhsAsDict = dynamic_cast<ScamDict *>(rhs);
        KeyVec const & keys = lhsAsDict->getKeys();

        for ( auto key : keys ) {
            ScamValue d = rhsAsDict->get(key);
            if ( error(d) ) {
                return make_common_error(writeValue(d).c_str());
            }
            ScamValue p = lhsAsDict->get(key);
            ScamValue result = exec(dict, p, d);
            if ( ! isDict(result) ) {
                return result;
            }
        }

        return dict;
    }

    return ExpressionFactory::makeNil();
}

ScamValue MatchUnifyCommon::exec(ScamDict * dict,
                                  ScamValue lhs,
                                  ScamValue rhs)
{
    using CheckerType =
        ScamValue (MatchUnifyCommon::*)(ScamDict*,
                                         ScamValue,
                                         ScamValue);

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
    ScamDict * dict = dynamic_cast<ScamDict *>(expr);
    Substitutor subst(dict);
    return subst.resolve_value(expr);
}
