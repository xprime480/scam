

#include "prim/Match.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamDict.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void do_match(ScamExpr * args, ContHandle cont);
    extern void do_unify(ScamExpr * args, ContHandle cont);
}

Match::Match()
    : Primitive("match")
{
}

void Match::applyArgs(ScamExpr * args, ContHandle cont)
{
    do_match(args, cont);
}

Unify::Unify()
    : Primitive("unify")
{
}

void Unify::applyArgs(ScamExpr * args, ContHandle cont)
{
    do_unify(args, cont);
}

namespace
{
    class MatchUnifyCommon
    {
    public:
        MatchUnifyCommon(ScamExpr * args, ContHandle cont, bool unify)
            : args(args->clone())
            , cont(cont)
            , unify(unify)
            , helper(ExpressionFactory::makeNil())
        {
        }

        void solve()
        {
            if (  checkargs() ) {
                process();
            }
        }

    private:
        ExprHandle args;
        ContHandle cont;
        bool       unify;
        ExprHandle helper;

        bool checkargs()
        {
            if ( args->length() < 2u ) {
                stringstream s;
                s << "match expected pattern data; got " << args->toString();
                ExprHandle err = ExpressionFactory::makeError(s.str());
                cont->run(err.get());
                return false;
            }
            return true;
        }

        void process()
        {
            ExprHandle lhs = args->nthcar(0);
            ExprHandle rhs = args->nthcar(1);
            ExprHandle rv = ExpressionFactory::makeDict();
            ScamDict * dict = dynamic_cast<ScamDict*>(rv.get());

            ExprHandle result = exec(dict, lhs.get(), rhs.get());
            if ( unify && result->isDict() ) {
                result = resolve(result.get());
            }
            cont->run(result.get());
        }

        ExprHandle make_error(string const & text)
        {
            ExprHandle msg = ExpressionFactory::makeString(text);
            ExprHandle rv = ExpressionFactory::makeBoolean(false);
            return ExpressionFactory::makeList(rv.get(), msg.get());
        }

        ExprHandle
        check_ignore(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
	    static ExprHandle ignore = ExpressionFactory::makeKeyword("::");

            if ( ignore->equals(lhs) || ignore->equals(rhs) ) {
                return dict->clone();
            }

            return ExpressionFactory::makeNil();
        }

        ExprHandle
        check_literals(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            if ( lhs->equals(rhs) ) {
                return dict->clone();
            }

            return ExpressionFactory::makeNil();
        }

        ExprHandle
        check_keyword(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
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
                    return make_error(s.str());
                }

                return dict->clone();
            }

            return ExpressionFactory::makeNil();
        }

        ExprHandle
        check_cons(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            if ( lhs->isCons() && rhs->isCons() ) {
                ExprHandle lhsCar  = lhs->getCar();
                ExprHandle rhsCar  = rhs->getCar();
                ExprHandle carMatch = exec(dict, lhsCar.get(), rhsCar.get());
                if ( ! carMatch->isDict() ) {
                    return carMatch;
                }

                ExprHandle lhsCdr  = lhs->getCdr();
                ExprHandle rhsCdr  = rhs->getCdr();
                return exec(dict, lhsCdr.get(), rhsCdr.get());
            }

            return ExpressionFactory::makeNil();
        }

        ExprHandle
        check_vector(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            if ( lhs->isVector() && rhs->isVector() ) {
                if ( lhs->length() != rhs->length() ) {
                    stringstream s;
                    s << "matching vectors of unequal length: "
                      << lhs->toString() << " and " << rhs->toString();
                    return make_error(s.str());
                }

                size_t len = lhs->length();
                for ( size_t idx = 0 ; idx < len ; ++ idx ) {
                    ExprHandle lhsN = lhs->nthcar(idx);
                    ExprHandle rhsN = rhs->nthcar(idx);
                    ExprHandle matchN = exec(dict, lhsN.get(), rhsN.get());
                    if ( ! matchN->isDict() ) {
                        return matchN;
                    }
                }

                return dict->clone();
            }

            return ExpressionFactory::makeNil();
        }

        ExprHandle
        check_dict(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            if ( lhs->isDict() && rhs->isDict() ) {
                if ( lhs->length() != rhs->length() ) {
                    stringstream s;
                    s << "dictionaries do not match " << lhs->toString()
                      << "; " << rhs->toString();
                    return make_error(s.str());
                }

                ScamDict * lhsAsDict = dynamic_cast<ScamDict *>(lhs);
                ScamDict * rhsAsDict = dynamic_cast<ScamDict *>(rhs);
                ExprVec const & keys = lhsAsDict->getKeys();

                for ( auto key : keys ) {
                    ExprHandle d = rhsAsDict->get(key.get());
                    if ( d->error() ) {
                        string const text = d->toString();
                        return make_error(text);
                    }
                    ExprHandle p = lhsAsDict->get(key.get());
                    ExprHandle result = exec(dict, p.get(), d.get());
                    if ( ! result->isDict() ) {
                        return result;
                    }
                }

                return dict->clone();
            }

            return ExpressionFactory::makeNil();
        }

        ExprHandle exec(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            ExprHandle rv;

	    rv = check_ignore(dict, lhs, rhs);
            if ( ! rv->isNil() ) {
                return rv;
            }

            rv = check_literals(dict, lhs, rhs);
            if ( ! rv->isNil() ) {
                return rv;
            }

            rv = check_keyword(dict, lhs, rhs);
            if ( ! rv->isNil() ) {
                return rv;
            }


            if ( unify ) {
                rv = check_keyword(dict, rhs, lhs);
                if ( ! rv->isNil() ) {
                    return rv;
                }
            }

            rv = check_cons(dict, lhs, rhs);
            if ( ! rv->isNil() ) {
                return rv;
            }

            rv = check_vector(dict, lhs, rhs);
            if ( ! rv->isNil() ) {
                return rv;
            }

            rv = check_dict(dict, lhs, rhs);
            if ( ! rv->isNil() ) {
                return rv;
            }

            stringstream s;
            s << "Pattern: " << lhs->toString()
              << " does not conform to data: " << rhs->toString();
            return make_error(s.str());
        }

        ExprHandle resolve_cons(ScamExpr * expr, ScamDict * answers)
        {
            ExprHandle car = expr->getCar();
            ExprHandle cdr = expr->getCdr();

            ExprHandle newCar = resolve_value(car.get(), answers);
            ExprHandle newCdr = resolve_value(cdr.get(), answers);
            return ExpressionFactory::makeCons(newCar.get(), newCdr.get());
        }

        ExprHandle resolve_vector(ScamExpr * expr, ScamDict * answers)
        {
            ExprVec newExprs;
            size_t const len = expr->length();

            for ( size_t idx = 0 ; idx < len ; ++idx ) {
                ExprHandle val = expr->nthcar(idx);
                ExprHandle newVal = resolve_value(val.get(), answers);
                newExprs.push_back(newVal);
            }

            ExprHandle rv = ExpressionFactory::makeVector(newExprs);
            return rv;
        }

        ExprHandle resolve_dict(ScamExpr * expr, ScamDict * answers)
        {
            if ( 0u == expr->length() ) {
                return expr->clone();
            }

            ExprHandle final = ExpressionFactory::makeDict();
            ScamDict * f    = dynamic_cast<ScamDict *>(final.get());
            ScamDict * dict = dynamic_cast<ScamDict *>(expr);

            ExprVec const & keys = dict->getKeys();
            for ( auto key : keys ) {
                ExprHandle val = dict->get(key.get());
                ExprHandle newVal = resolve_value(val.get(), answers);
                if ( newVal->error() ) {
                    return newVal;
                }
                f->put(key.get(), newVal.get());
            }

            return final;
        }

        bool have_seen(ScamExpr * expr)
        {
            ExprHandle t = helper;
            while ( ! t->isNil() ) {
                if ( t->nthcar(0)->equals(expr) ) {
                    return true;
                }
                t = t->nthcdr(0);
            }

            return false;
        }

        ExprHandle resolve_keyword(ScamExpr * expr, ScamDict * answers)
        {
            if ( have_seen(expr) ) {
                stringstream s;
                s << "Infinite Loop resolving keyword " << expr->toString();
                return make_error(s.str());
            }

            helper = ExpressionFactory::makeCons(expr, helper.get());

            ExprHandle val = answers->get(expr);
            ExprHandle rv  = resolve_value(val.get(), answers);

            helper = helper->nthcdr(0);
            return rv;
        }

        ExprHandle resolve_value(ScamExpr * expr, ScamDict * answers)
        {
            ExprHandle rv;

            if ( expr->isCons() ) {
                rv = resolve_cons(expr, answers);
            }
            else if ( expr->isVector() ) {
                rv = resolve_vector(expr, answers);
            }
            else if ( expr->isDict() ) {
                rv = resolve_dict(expr, answers);
            }
            else if ( expr->isKeyword() ) {
                rv = resolve_keyword(expr, answers);
            }
            else {
                rv = expr->clone();
            }

            return rv;
        }

        ExprHandle resolve(ScamExpr * expr)
        {
            ScamDict * dict = dynamic_cast<ScamDict *>(expr);
            return resolve_dict(expr, dict);
        }
    };

    void do_match(ScamExpr * args, ContHandle cont)
    {
        MatchUnifyCommon solver(args, cont, false);
        solver.solve();
    }

    void do_unify(ScamExpr * args, ContHandle cont)
    {
        MatchUnifyCommon solver(args, cont, true);
        solver.solve();
    }
}
