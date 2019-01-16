

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

Substitute::Substitute()
    : Primitive("substitute")
{
}

void Substitute::applyArgs(ScamExpr * args, ContHandle cont)
{
    do_subst(args, cont);
}

Instantiate::Instantiate()
    : Primitive("instantiate")
{
}

void Instantiate::applyArgs(ScamExpr * args, ContHandle cont)
{
    do_inst(args, cont, counter);
}

size_t Instantiate::counter { 0 };

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

    void do_subst(ScamExpr * args, ContHandle cont)
    {
        if ( args->length() < 2 ) {
            stringstream s;
            s << "expected 2 args; got " << args->length();
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
            return;
        }

        ExprHandle form = args->nthcar(0);
        ExprHandle dict = args->nthcar(1);
        ScamDict * answers = dynamic_cast<ScamDict *>(dict.get());
        if ( ! answers ) {
            stringstream s;
            s << "expected 'form dict'; got " << args->toString();
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
            return;
        }

        MatchUnifyCommon solver(args, cont, false);
        ExprHandle rv = solver.resolve_value(form.get(), answers);
        cont->run(rv.get());
    }

    class Instantiator
    {
    public:
        Instantiator(size_t & counter)
            : counter(counter)
        {
        }

        ExprHandle exec(ScamExpr * args)
        {
            bool ok;
            ExprHandle expr = checkargs(args, ok);
            if ( ! ok ) {
                return expr;
            }

            return inst_value(expr.get());
        }

    private:
        size_t & counter;
        ScamDict dict;

        ExprHandle make_error(ScamExpr * args)
        {
            stringstream s;
            s << "Substitute expected one arg got: ";
            s << args->toString();
            return ExpressionFactory::makeError(s.str());
        }

        ExprHandle checkargs(ScamExpr * args, bool & ok)
        {
            if ( args->length() < 1u ) {
                ok = false;
                return make_error(args);
            }

            ExprHandle expr = args->nthcar(0);
            ok = true;
            return expr;
        }

        ExprHandle inst_value(ScamExpr * expr)
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
                return expr->clone();
            }
        }

        ExprHandle new_mapping(ScamExpr * expr)
        {
            stringstream s;
            s << ":kw" << ++counter;
            ExprHandle value = ExpressionFactory::makeKeyword(s.str());
            dict.put(expr, value.get());
            return value;
        }

        ExprHandle inst_keyword(ScamExpr * expr)
        {
            if ( dict.has(expr) ) {
                return dict.get(expr);
            }
            else {
                return new_mapping(expr);
            }
        }

        ExprHandle inst_cons(ScamExpr * expr)
        {
            ExprHandle a0 = expr->nthcar(0);
            ExprHandle b0 = expr->nthcdr(0);
            ExprHandle a1 = inst_value(a0.get());
            ExprHandle b1 = inst_value(b0.get());
            return ExpressionFactory::makeCons(a1.get(), b1.get());
        }

        ExprHandle inst_vector(ScamExpr * expr)
        {
            size_t len = expr->length();
            if ( 0 == len ) {
                return expr->clone();
            }

            ExprVec elems;
            for ( size_t idx = 0 ; idx < len ; ++idx ) {
                ExprHandle i = expr->nthcar(idx);
                ExprHandle o = inst_value(i.get());
                elems.push_back(o);
            }

            return ExpressionFactory::makeVector(elems);
        }

        ExprHandle inst_dict(ScamExpr * expr)
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
                ExprHandle newVal = inst_value(val.get());
                f->put(key.get(), newVal.get());
            }

            return final;
        }
    };

    void do_inst(ScamExpr * args, ContHandle cont, size_t & counter)
    {
        Instantiator inst(counter);
        ExprHandle rv = inst.exec(args);
        cont->run(rv.get());
    }
}
