#include "prim/MatchUnifyCommon.hpp"

#include "Continuation.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ScamDict.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"
#include "input/MatchUnifyParser.hpp"
#include "prim/Substitutor.hpp"
#include "prim/CommonError.hpp"

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
    if ( ! parser->isMatch() && TypePredicates::isDict(result) ) {
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
    if ( TypePredicates::isKeyword(lhs) && ! TypePredicates::isNil(rhs) ) {
        ScamValue old = dict->get(lhs);
        if ( TypePredicates::error(old) ) {
            dict->put(lhs, rhs);
        }
        else if ( ! old->equals(rhs) ) {
            stringstream s;
            s << "Previous data for pattern " << ExprWriter::write(lhs)
              << " was '" << ExprWriter::write(old)
              << "'; does not match new data '"
              << ExprWriter::write(rhs) << "'";
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
    if ( TypePredicates::isCons(lhs) && TypePredicates::isCons(rhs) ) {
        ScamValue lhsCar  = lhs->getCar();
        ScamValue rhsCar  = rhs->getCar();
        ScamValue carMatch = exec(dict, lhsCar, rhsCar);
        if ( ! TypePredicates::isDict(carMatch) ) {
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
    if ( TypePredicates::isVector(lhs) && TypePredicates::isVector(rhs) ) {
        if ( lhs->length() != rhs->length() ) {
            stringstream s;
            s << "matching vectors of unequal length: "
              << ExprWriter::write(lhs) << " and " << ExprWriter::write(rhs);
            return make_common_error(s.str().c_str());
        }

        size_t len = lhs->length();
        for ( size_t idx = 0 ; idx < len ; ++ idx ) {
            ScamValue lhsN = lhs->nthcar(idx);
            ScamValue rhsN = rhs->nthcar(idx);
            ScamValue matchN = exec(dict, lhsN, rhsN);
            if ( ! TypePredicates::isDict(matchN) ) {
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
    if ( TypePredicates::isDict(lhs) && TypePredicates::isDict(rhs) ) {
        if ( lhs->length() != rhs->length() ) {
            stringstream s;
            s << "dictionaries do not match " << ExprWriter::write(lhs)
              << "; " << ExprWriter::write(rhs);
            return make_common_error(s.str().c_str());
        }

        ScamDict * lhsAsDict = dynamic_cast<ScamDict *>(lhs);
        ScamDict * rhsAsDict = dynamic_cast<ScamDict *>(rhs);
        KeyVec const & keys = lhsAsDict->getKeys();

        for ( auto key : keys ) {
            ScamValue d = rhsAsDict->get(key);
            if ( TypePredicates::error(d) ) {
                return make_common_error(ExprWriter::write(d).c_str());
            }
            ScamValue p = lhsAsDict->get(key);
            ScamValue result = exec(dict, p, d);
            if ( ! TypePredicates::isDict(result) ) {
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
        if ( ! TypePredicates::isNil(rv) ) {
          return rv;
        }
    }

    stringstream s;
    s << "Pattern: " << ExprWriter::write(lhs)
      << " does not conform to data: " << ExprWriter::write(rhs);
    return make_common_error(s.str().c_str());
}

ScamValue MatchUnifyCommon::resolve(ScamValue expr)
{
    ScamDict * dict = dynamic_cast<ScamDict *>(expr);
    Substitutor subst(dict);
    return subst.resolve_value(expr);
}
