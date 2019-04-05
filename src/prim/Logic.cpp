

#include "prim/Logic.hpp"

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
    extern void do_subst(ScamExpr * args, ContHandle cont);
    extern void do_inst(ScamExpr * args, ContHandle cont, size_t & counter);
}

Match::Match()
    : Primitive("match")
{
}

Match * Match::makeInstance()
{
    return new Match();
}

void Match::applyArgs(ScamExpr * args, ContHandle cont)
{
    do_match(args, cont);
}

Unify::Unify()
    : Primitive("unify")
{
}

Unify * Unify::makeInstance()
{
    return new Unify();
}

void Unify::applyArgs(ScamExpr * args, ContHandle cont)
{
    do_unify(args, cont);
}

Substitute::Substitute()
    : Primitive("substitute")
{
}

Substitute * Substitute::makeInstance()
{
    return new Substitute();
}

void Substitute::applyArgs(ScamExpr * args, ContHandle cont)
{
    do_subst(args, cont);
}

Instantiate::Instantiate()
    : Primitive("instantiate")
{
}

Instantiate * Instantiate::makeInstance()
{
    return new Instantiate();
}

void Instantiate::applyArgs(ScamExpr * args, ContHandle cont)
{
    do_inst(args, cont, counter);
}

size_t Instantiate::counter { 0 };

namespace
{
    ScamExpr * make_common_error(string const & text)
    {
        ScamExpr * msg = ExpressionFactory::makeString(text);
        ScamExpr * rv = ExpressionFactory::makeBoolean(false);
        return ExpressionFactory::makeList(rv, msg);
    }

    class ValueMapper
    {
    protected:

        ScamExpr * map_dict(ScamExpr * expr)
        {
            if ( 0u == expr->length() ) {
                return expr;
            }

            ScamExpr * rv   = ExpressionFactory::makeDict();
            ScamDict * f    = dynamic_cast<ScamDict *>(rv);
            ScamDict * dict = dynamic_cast<ScamDict *>(expr);

            KeyVec const & keys = dict->getKeys();
            for ( auto key : keys ) {
                ScamExpr * val = dict->get(key);
                ScamExpr * newVal = map_value(val);
                if ( newVal->error() ) {
                    return newVal;
                }
                f->put(key, newVal);
            }

            return rv;
        }

        ScamExpr * map_vector(ScamExpr * expr)
        {
            ExprVec newExprs;
            size_t const len = expr->length();

            for ( size_t idx = 0 ; idx < len ; ++idx ) {
                ScamExpr * val = expr->nthcar(idx);
                ScamExpr * newVal = map_value(val);
                newExprs.push_back(newVal);
            }

            ScamExpr * rv = ExpressionFactory::makeVector(newExprs);
            return rv;
        }

        ScamExpr * map_cons(ScamExpr * expr)
        {
            ScamExpr * head = expr->nthcar(0);
            ScamExpr * tail = expr->nthcdr(0);
            ScamExpr * newHead = map_value(head);
            ScamExpr * newTail = map_value(tail);
            return ExpressionFactory::makeCons(newHead, newTail);
        }

        virtual ScamExpr * map_value(ScamExpr * expr) = 0;
    };

    class Substitutor : public ValueMapper
    {
    public:
        Substitutor(ScamDict * answers)
            : answers(answers)
            , helper(ExpressionFactory::makeNil())
        {
        }

        ScamExpr * resolve_value(ScamExpr * expr)
        {
            ScamExpr * rv;

            if ( expr->isCons() ) {
                rv = resolve_cons(expr);
            }
            else if ( expr->isVector() ) {
                rv = resolve_vector(expr);
            }
            else if ( expr->isDict() ) {
                rv = resolve_dict(expr);
            }
            else if ( expr->isKeyword() ) {
                rv = resolve_keyword(expr);
            }
            else {
                rv = expr;
            }

            return rv;
        }

    protected:
        ScamExpr * map_value(ScamExpr * val) override
        {
            return resolve_value(val);
        }

    private:
        ScamDict * answers;
        ScamExpr * helper;

        ScamExpr * resolve_cons(ScamExpr * expr)
        {
            return map_cons(expr);
        }

        ScamExpr * resolve_vector(ScamExpr * expr)
        {
            return map_vector(expr);
        }

        bool have_seen(ScamExpr * expr)
        {
            ScamExpr * t = helper;
            while ( ! t->isNil() ) {
                if ( t->nthcar(0)->equals(expr) ) {
                    return true;
                }
                t = t->nthcdr(0);
            }

            return false;
        }

        ScamExpr * resolve_keyword(ScamExpr * expr)
        {
            if ( have_seen(expr) ) {
                stringstream s;
                s << "Infinite Loop resolving keyword " << expr->toString();
                return make_common_error(s.str());
            }

            helper = ExpressionFactory::makeCons(expr, helper);

            ScamExpr * val = answers->get(expr);
            ScamExpr * rv  = resolve_value(val);

            helper = helper->nthcdr(0);
            return rv;
        }

        ScamExpr * resolve_dict(ScamExpr * expr)
        {
            return map_dict(expr);
        }
    };

    class MatchUnifyCommon
    {
    public:
        MatchUnifyCommon(ScamExpr * args, ContHandle cont, bool unify)
            : args(args)
            , cont(cont)
            , unify(unify)
        {
        }

        void solve()
        {
            if (  checkargs() ) {
                process();
            }
        }

    private:
        ScamExpr * args;
        ContHandle cont;
        bool       unify;

        bool checkargs()
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

        void process()
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

        ScamExpr *
        check_ignore(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            static ScamExpr * ignore = ExpressionFactory::makeKeyword("::");

            if ( ignore->equals(lhs) || ignore->equals(rhs) ) {
                return dict;
            }

            return ExpressionFactory::makeNil();
        }

        ScamExpr *
        check_literals(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            if ( lhs->equals(rhs) ) {
                return dict;
            }

            return ExpressionFactory::makeNil();
        }

        ScamExpr *
        check_keyword(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
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
                    return make_common_error(s.str());
                }

                return dict;
            }

            return ExpressionFactory::makeNil();
        }

        ScamExpr *
        check_keyword_reversed(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            if ( unify ) {
                return check_keyword(dict, rhs, lhs);
            }
            else {
                return ExpressionFactory::makeNil();
            }
        }

        ScamExpr *
        check_cons(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
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

        ScamExpr *
        check_vector(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            if ( lhs->isVector() && rhs->isVector() ) {
                if ( lhs->length() != rhs->length() ) {
                    stringstream s;
                    s << "matching vectors of unequal length: "
                      << lhs->toString() << " and " << rhs->toString();
                    return make_common_error(s.str());
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

        ScamExpr *
        check_dict(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            if ( lhs->isDict() && rhs->isDict() ) {
                if ( lhs->length() != rhs->length() ) {
                    stringstream s;
                    s << "dictionaries do not match " << lhs->toString()
                      << "; " << rhs->toString();
                    return make_common_error(s.str());
                }

                ScamDict * lhsAsDict = dynamic_cast<ScamDict *>(lhs);
                ScamDict * rhsAsDict = dynamic_cast<ScamDict *>(rhs);
                KeyVec const & keys = lhsAsDict->getKeys();

                for ( auto key : keys ) {
                    ScamExpr * d = rhsAsDict->get(key);
                    if ( d->error() ) {
                        string const text = d->toString();
                        return make_common_error(text);
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

        ScamExpr * exec(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs)
        {
            using CheckerType =
                ScamExpr * (MatchUnifyCommon::*)(ScamDict*, ScamExpr*, ScamExpr*);

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
            return make_common_error(s.str());
        }

        ScamExpr * resolve(ScamExpr * expr)
        {
            ScamDict * dict = dynamic_cast<ScamDict *>(expr);
            Substitutor subst(dict);
            return subst.resolve_value(expr);
        }
    };

    class Instantiator : public ValueMapper
    {
    public:
        Instantiator(size_t & counter)
            : counter(counter)
        {
        }

        ScamExpr * exec(ScamExpr * args)
        {
            bool ok;
            ScamExpr * expr = checkargs(args, ok);
            if ( ! ok ) {
                return expr;
            }

            return inst_value(expr);
        }

    protected:
        ScamExpr * map_value(ScamExpr * val)
        {
            return inst_value(val);
        }

    private:
        size_t & counter;
        ScamDict dict;

        ScamExpr * make_error(ScamExpr * args)
        {
            stringstream s;
            s << "Substitute expected one arg got: ";
            s << args->toString();
            return ExpressionFactory::makeError(s.str());
        }

        ScamExpr * checkargs(ScamExpr * args, bool & ok)
        {
            if ( args->length() < 1u ) {
                ok = false;
                return make_error(args);
            }

            ScamExpr * expr = args->nthcar(0);
            ok = true;
            return expr;
        }

        ScamExpr * inst_value(ScamExpr * expr)
        {
            if ( expr->isKeyword() ) {
                return inst_keyword(expr);
            }
            else if ( expr->isCons() ) {
                return inst_cons(expr);
            }
            else if ( expr->isVector() ) {
                return inst_vector(expr);
            }
            else if ( expr->isDict() ) {
                return inst_dict(expr);
            }
            else {
                return expr;
            }
        }

        ScamExpr * new_mapping(ScamExpr * expr)
        {
            stringstream s;
            s << ":kw" << ++counter;
            ScamExpr * value = ExpressionFactory::makeKeyword(s.str());
            dict.put(expr, value);
            return value;
        }

        ScamExpr * inst_keyword(ScamExpr * expr)
        {
            if ( dict.has(expr) ) {
                return dict.get(expr);
            }
            else {
                return new_mapping(expr);
            }
        }

        ScamExpr * inst_cons(ScamExpr * expr)
        {
            return map_cons(expr);
        }

        ScamExpr * inst_vector(ScamExpr * expr)
        {
            return map_vector(expr);
        }

        ScamExpr * inst_dict(ScamExpr * expr)
        {
            return map_dict(expr);
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

    void do_subst(ScamExpr * args, ContHandle cont)
    {
        if ( args->length() < 2 ) {
            stringstream s;
            s << "expected 2 args; got " << args->length();
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
            return;
        }

        ScamExpr * form = args->nthcar(0);
        ScamExpr * dict = args->nthcar(1);
        ScamDict * answers = dynamic_cast<ScamDict *>(dict);
        if ( ! answers ) {
            stringstream s;
            s << "expected 'form dict'; got " << args->toString();
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
            return;
        }

        Substitutor resolver(answers);
        ScamExpr * rv = resolver.resolve_value(form);
        cont->run(rv);
    }

    void do_inst(ScamExpr * args, ContHandle cont, size_t & counter)
    {
        Instantiator inst(counter);
        ScamExpr * rv = inst.exec(args);
        cont->run(rv);
    }
}
