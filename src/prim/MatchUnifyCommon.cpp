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

MatchUnifyCommon::MatchUnifyCommon(ExprHandle args,
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
        ExprHandle err = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return false;
    }
    return true;
}

void MatchUnifyCommon::process()
{
    ExprHandle lhs = args->nthcar(0);
    ExprHandle rhs = args->nthcar(1);
    ExprHandle rv = ExpressionFactory::makeDict();

    if ( unify && args->length() > 2u ) {
        rv = args->nthcar(2);
    }

    ScamDict * dict = dynamic_cast<ScamDict*>(rv);
    if ( nullptr == dict ) {
        ExprHandle err =
            ExpressionFactory::makeError("Unify expected dict for third arg",
                                         "; got ",
                                         rv->toString());
        cont->run(err);
        return;
    }

    ExprHandle result = exec(dict, lhs, rhs);
    if ( unify && result->isDict() ) {
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
    if ( lhs->isKeyword() && ! rhs->isNil() ) {
        ExprHandle old = dict->get(lhs);
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

ExprHandle MatchUnifyCommon::check_keyword_reversed(ScamDict * dict,
                                                    ExprHandle lhs,
                                                    ExprHandle rhs)
{
    if ( unify ) {
        return check_keyword(dict, rhs, lhs);
    }
    else {
        return ExpressionFactory::makeNil();
    }
}

ExprHandle MatchUnifyCommon::check_cons(ScamDict * dict,
                                        ExprHandle lhs,
                                        ExprHandle rhs)
{
    if ( lhs->isCons() && rhs->isCons() ) {
        ExprHandle lhsCar  = lhs->getCar();
        ExprHandle rhsCar  = rhs->getCar();
        ExprHandle carMatch = exec(dict, lhsCar, rhsCar);
        if ( ! carMatch->isDict() ) {
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
    if ( lhs->isVector() && rhs->isVector() ) {
        if ( lhs->length() != rhs->length() ) {
            stringstream s;
            s << "matching vectors of unequal length: "
              << lhs->toString() << " and " << rhs->toString();
            return make_common_error(s.str().c_str());
        }

        size_t len = lhs->length();
        for ( size_t idx = 0 ; idx < len ; ++ idx ) {
            ExprHandle lhsN = lhs->nthcar(idx);
            ExprHandle rhsN = rhs->nthcar(idx);
            ExprHandle matchN = exec(dict, lhsN, rhsN);
            if ( ! matchN->isDict() ) {
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
            ExprHandle d = rhsAsDict->get(key);
            if ( d->error() ) {
                return make_common_error(d->toString().c_str());
            }
            ExprHandle p = lhsAsDict->get(key);
            ExprHandle result = exec(dict, p, d);
            if ( ! result->isDict() ) {
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
        if ( ! rv->isNil() ) {
          return rv;
        }
    }

    stringstream s;
    s << "Pattern: " << lhs->toString()
      << " does not conform to data: " << rhs->toString();
    return make_common_error(s.str().c_str());
}

ExprHandle MatchUnifyCommon::resolve(ExprHandle expr)
{
    ScamDict * dict = dynamic_cast<ScamDict *>(expr);
    Substitutor subst(dict);
    return subst.resolve_value(expr);
}
