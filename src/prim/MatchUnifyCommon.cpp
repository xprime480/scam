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
    ExprHandle lhs  = parser->getLhs();
    ExprHandle rhs  = parser->getRhs();
    ScamDict * dict = parser->getDict();

    ExprHandle result = exec(dict, lhs, rhs);
    if ( ! parser->isMatch() && TypePredicates::isDict(result) ) {
        result = resolve(result);
    }

    cont->run(result);
}

ExprHandle MatchUnifyCommon::check_ignore(ScamDict * dict,
                                          ExprHandle lhs,
                                          ExprHandle rhs)
{
    static ExprHandle ignore =
        ExpressionFactory::makeKeyword("::", false);

    if ( ignore->equals(lhs) || ignore->equals(rhs) ) {
        return dict;
    }

    return ExpressionFactory::makeNil();
}

ExprHandle MatchUnifyCommon::check_literals(ScamDict * dict,
                                            ExprHandle lhs,
                                            ExprHandle rhs)
{
    if ( lhs->equals(rhs) ) {
        return dict;
    }

    return ExpressionFactory::makeNil();
}

ExprHandle MatchUnifyCommon::check_keyword(ScamDict * dict,
                                           ExprHandle lhs,
                                           ExprHandle rhs)
{
    if ( TypePredicates::isKeyword(lhs) && ! TypePredicates::isNil(rhs) ) {
        ExprHandle old = dict->get(lhs);
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

ExprHandle MatchUnifyCommon::check_keyword_reversed(ScamDict * dict,
                                                    ExprHandle lhs,
                                                    ExprHandle rhs)
{
    if ( parser->isMatch() ) {
        return ExpressionFactory::makeNil();
    }
    else {
        return check_keyword(dict, rhs, lhs);
    }
}

ExprHandle MatchUnifyCommon::check_cons(ScamDict * dict,
                                        ExprHandle lhs,
                                        ExprHandle rhs)
{
    if ( TypePredicates::isCons(lhs) && TypePredicates::isCons(rhs) ) {
        ExprHandle lhsCar  = lhs->getCar();
        ExprHandle rhsCar  = rhs->getCar();
        ExprHandle carMatch = exec(dict, lhsCar, rhsCar);
        if ( ! TypePredicates::isDict(carMatch) ) {
            return carMatch;
        }

        ExprHandle lhsCdr  = lhs->getCdr();
        ExprHandle rhsCdr  = rhs->getCdr();
        return exec(dict, lhsCdr, rhsCdr);
    }

    return ExpressionFactory::makeNil();
}

ExprHandle MatchUnifyCommon::check_vector(ScamDict * dict,
                                          ExprHandle lhs,
                                          ExprHandle rhs)
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
            ExprHandle lhsN = lhs->nthcar(idx);
            ExprHandle rhsN = rhs->nthcar(idx);
            ExprHandle matchN = exec(dict, lhsN, rhsN);
            if ( ! TypePredicates::isDict(matchN) ) {
                return matchN;
            }
        }

        return dict;
    }

    return ExpressionFactory::makeNil();
}

ExprHandle MatchUnifyCommon::check_dict(ScamDict * dict,
                                        ExprHandle lhs,
                                        ExprHandle rhs)
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
            ExprHandle d = rhsAsDict->get(key);
            if ( TypePredicates::error(d) ) {
                return make_common_error(ExprWriter::write(d).c_str());
            }
            ExprHandle p = lhsAsDict->get(key);
            ExprHandle result = exec(dict, p, d);
            if ( ! TypePredicates::isDict(result) ) {
                return result;
            }
        }

        return dict;
    }

    return ExpressionFactory::makeNil();
}

ExprHandle MatchUnifyCommon::exec(ScamDict * dict,
                                  ExprHandle lhs,
                                  ExprHandle rhs)
{
    using CheckerType =
        ExprHandle (MatchUnifyCommon::*)(ScamDict*,
                                         ExprHandle,
                                         ExprHandle);

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
        ExprHandle rv = (this->*checker)(dict, lhs, rhs);
        if ( ! TypePredicates::isNil(rv) ) {
          return rv;
        }
    }

    stringstream s;
    s << "Pattern: " << ExprWriter::write(lhs)
      << " does not conform to data: " << ExprWriter::write(rhs);
    return make_common_error(s.str().c_str());
}

ExprHandle MatchUnifyCommon::resolve(ExprHandle expr)
{
    ScamDict * dict = dynamic_cast<ScamDict *>(expr);
    Substitutor subst(dict);
    return subst.resolve_value(expr);
}
