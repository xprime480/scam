
#include "prim/MatchUnifyCommon.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ScamDict.hpp"
#include "expr/ExpressionFactory.hpp"
#include "prim/Substitutor.hpp"
#include "prim/CommonError.hpp"

#include <sstream>
#include <string>

using namespace scam;
using namespace std;

MatchUnifyCommon::MatchUnifyCommon(ScamExpr * args,
                                   Continuation * cont,
                                   bool unify)
    : args(args)
    , cont(cont)
    , unify(unify)
{
}

void MatchUnifyCommon::solve()
{
    if (  checkargs() ) {
        process();
    }
}

bool MatchUnifyCommon::checkargs()
{
    if ( args->length() < 2u ) {
        stringstream s;
        if ( unify ) {
            s << "unify";
        }
        else {
            s << "match";
        }

        s << "expected pattern data";
        if ( unify ) {
            s << " [dict]";
        }

        s << "; got " << args->toString();
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return false;
    }
    return true;
}

void MatchUnifyCommon::process()
{
    ScamExpr * lhs = args->nthcar(0);
    ScamExpr * rhs = args->nthcar(1);
    ScamExpr * rv = ExpressionFactory::makeDict();

    if ( unify && args->length() > 2u ) {
        rv = args->nthcar(2);
    }

    ScamDict * dict = dynamic_cast<ScamDict*>(rv);
    if ( nullptr == dict ) {
        stringstream s;
        s << "Unify expected dict for third arg; got "
          << rv->toString();
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return;
    }

    ScamExpr * result = exec(dict, lhs, rhs);
    if ( unify && result->isDict() ) {
      result = resolve(result);
    }
    cont->run(result);
}

ScamExpr * MatchUnifyCommon::check_ignore(ScamDict * dict,
                                          ScamExpr * lhs,
                                          ScamExpr * rhs)
{
    static ScamExpr * ignore =
        ExpressionFactory::makeKeyword("::", false);

    if ( ignore->equals(lhs) || ignore->equals(rhs) ) {
        return dict;
    }

    return ExpressionFactory::makeNil();
}

ScamExpr * MatchUnifyCommon::check_literals(ScamDict * dict,
                                            ScamExpr * lhs,
                                            ScamExpr * rhs)
{
    if ( lhs->equals(rhs) ) {
        return dict;
    }

    return ExpressionFactory::makeNil();
}

ScamExpr * MatchUnifyCommon::check_keyword(ScamDict * dict,
                                           ScamExpr * lhs,
                                           ScamExpr * rhs)
{
    if ( lhs->isKeyword() && ! rhs->isNil() ) {
        ScamExpr * old = dict->get(lhs);
        if ( old->error() ) {
            dict->put(lhs, rhs);
        }
        else if ( ! old->equals(rhs) ) {
            stringstream s;
            s << "Previous data for pattern " << lhs->toString()
              << " was '" << old->toString()
              << "'; does not match new data '"
              << rhs->toString() << "'";
            return make_common_error(s.str().c_str());
        }

        return dict;
    }

    return ExpressionFactory::makeNil();
}

ScamExpr * MatchUnifyCommon::check_keyword_reversed(ScamDict * dict,
                                                    ScamExpr * lhs,
                                                    ScamExpr * rhs)
{
    if ( unify ) {
        return check_keyword(dict, rhs, lhs);
    }
    else {
        return ExpressionFactory::makeNil();
    }
}

ScamExpr * MatchUnifyCommon::check_cons(ScamDict * dict,
                                        ScamExpr * lhs,
                                        ScamExpr * rhs)
{
    if ( lhs->isCons() && rhs->isCons() ) {
        ScamExpr * lhsCar  = lhs->getCar();
        ScamExpr * rhsCar  = rhs->getCar();
        ScamExpr * carMatch = exec(dict, lhsCar, rhsCar);
        if ( ! carMatch->isDict() ) {
            return carMatch;
        }

        ScamExpr * lhsCdr  = lhs->getCdr();
        ScamExpr * rhsCdr  = rhs->getCdr();
        return exec(dict, lhsCdr, rhsCdr);
    }

    return ExpressionFactory::makeNil();
}

ScamExpr * MatchUnifyCommon::check_vector(ScamDict * dict,
                                          ScamExpr * lhs,
                                          ScamExpr * rhs)
{
    if ( lhs->isVector() && rhs->isVector() ) {
        if ( lhs->length() != rhs->length() ) {
            stringstream s;
            s << "matching vectors of unequal length: "
              << lhs->toString() << " and " << rhs->toString();
            return make_common_error(s.str().c_str());
        }

        size_t len = lhs->length();
        for ( size_t idx = 0 ; idx < len ; ++ idx ) {
            ScamExpr * lhsN = lhs->nthcar(idx);
            ScamExpr * rhsN = rhs->nthcar(idx);
            ScamExpr * matchN = exec(dict, lhsN, rhsN);
            if ( ! matchN->isDict() ) {
                return matchN;
            }
        }

        return dict;
    }

    return ExpressionFactory::makeNil();
}

ScamExpr * MatchUnifyCommon::check_dict(ScamDict * dict,
                                        ScamExpr * lhs,
                                        ScamExpr * rhs)
{
    if ( lhs->isDict() && rhs->isDict() ) {
        if ( lhs->length() != rhs->length() ) {
            stringstream s;
            s << "dictionaries do not match " << lhs->toString()
              << "; " << rhs->toString();
            return make_common_error(s.str().c_str());
        }

        ScamDict * lhsAsDict = dynamic_cast<ScamDict *>(lhs);
        ScamDict * rhsAsDict = dynamic_cast<ScamDict *>(rhs);
        KeyVec const & keys = lhsAsDict->getKeys();

        for ( auto key : keys ) {
            ScamExpr * d = rhsAsDict->get(key);
            if ( d->error() ) {
                return make_common_error(d->toString().c_str());
            }
            ScamExpr * p = lhsAsDict->get(key);
            ScamExpr * result = exec(dict, p, d);
            if ( ! result->isDict() ) {
                return result;
            }
        }

        return dict;
    }

    return ExpressionFactory::makeNil();
}

ScamExpr * MatchUnifyCommon::exec(ScamDict * dict,
                                  ScamExpr * lhs,
                                  ScamExpr * rhs)
{
    using CheckerType =
        ScamExpr * (MatchUnifyCommon::*)(ScamDict*,
                                         ScamExpr*,
                                         ScamExpr*);

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
        ScamExpr * rv = (this->*checker)(dict, lhs, rhs);
        if ( ! rv->isNil() ) {
          return rv;
        }
    }

    stringstream s;
    s << "Pattern: " << lhs->toString()
      << " does not conform to data: " << rhs->toString();
    return make_common_error(s.str().c_str());
}

ScamExpr * MatchUnifyCommon::resolve(ScamExpr * expr)
{
    ScamDict * dict = dynamic_cast<ScamDict *>(expr);
    Substitutor subst(dict);
    return subst.resolve_value(expr);
}
