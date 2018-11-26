
#include "prim/EqualP.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void equal_p_impl(ScamExpr * args, ContHandle cont);
}

EqualP::EqualP()
    : Primitive("eq?")
{
}

void EqualP::applyArgs(ScamExpr * args, ContHandle cont)
{
    equal_p_impl(args, cont);
}

namespace
{
    static const ExprHandle yes = ExpressionFactory::makeBoolean(true);
    static const ExprHandle no  = ExpressionFactory::makeBoolean(false);

    extern bool compare_all(ScamExpr * args);

    template <typename MapFn>
    ExprHandle apply_map(ScamExpr * args, MapFn mapper)
    {
        if ( args->isNil() ) {
            return ExpressionFactory::makeNil();
        }
        if ( ! args->isCons() ) {
            return mapper(args);
        }

        ExprHandle car = args->nthcar(0);
        ExprHandle cdr = args->nthcdr(0);

        ExprHandle newCar = mapper(car.get());
        ExprHandle newCdr = apply_map(cdr.get(), mapper);

        return ExpressionFactory::makeCons(newCar.get(), newCdr.get());
    }

    template <typename MapFn, typename ReduceFn>
    ExprHandle map_reduce(ScamExpr * args, MapFn mapper, ReduceFn reducer)
    {
        ExprHandle mapped = apply_map(args, mapper);
        ExprHandle reduced = reducer(mapped.get());
        return reduced;
    }

    bool zero_args(ScamExpr * args, ContHandle cont)
    {
        if ( 0 == args->length() ) {
            cont->run(yes.get());
            return true;
        }
        return false;
    }

    ExprHandle all(ScamExpr * args)
    {
        const size_t len = args->length();
        for ( size_t idx = 0 ; idx < len ; ++idx ) {
            ExprHandle arg = args->nthcar(idx);
            if ( ! arg->truth() ) {
                return no;
            }
        }

        return yes;
    }

    ExprHandle any(ScamExpr * args)
    {
        const size_t len = args->length();
        for ( size_t idx = 0 ; idx < len ; ++idx ) {
            if ( args->nthcar(idx)->truth() ) {
                return yes;
            }
        }
        return no;
    }

    bool has_nulls(ScamExpr * args, ContHandle cont)
    {
        auto fn = [](ScamExpr * arg) -> ExprHandle {
            return ExpressionFactory::makeBoolean(arg->isNull());
        };
        ExprHandle answer = map_reduce(args, fn, any);
        return answer->truth();
    }

    bool one_arg(ScamExpr * args, ContHandle cont)
    {
        if ( 1 == args->length() ) {
            cont->run(yes.get());
            return true;
        }
        return false;
    }

    using ExprPredicate = bool (ScamExpr::*)() const;

    bool all_same_type(ScamExpr * args, ExprPredicate pred)
    {
        auto fn = [=](ScamExpr * arg) -> ExprHandle {
            bool rv = (arg->*pred)();
            return ExpressionFactory::makeBoolean(rv);
        };
        ExprHandle answer = map_reduce(args, fn, all);
        return answer->truth();
    }

    template <typename T>
    struct MatchMaker
    {
        using ConverterType = T (ScamExpr::*)() const;

        MatchMaker(ConverterType converter)
            : converter(converter)
        {
        }

        ExprHandle operator()(ScamExpr * args)
        {
            T const first = ((args->nthcar(0).get())->*converter)();
            size_t len = args->length();

            for ( size_t idx = 1 ; idx < len ; ++idx ) {
                T const curr = ((args->nthcar(idx).get())->*converter)();
                if ( first != curr ) {
                    return no;
                }
            }

            return yes;
        }

        ConverterType converter;
    };

    bool all_same_length(ScamExpr * args)
    {
        auto mapper = [=](ScamExpr * arg) -> ExprHandle {
            size_t len = arg->length();
            return ExpressionFactory::makeInteger(len);
        };
        MatchMaker<string> reducer(&ScamExpr::toString);

        ExprHandle answer = map_reduce(args, mapper, reducer);
        return answer->truth();
    }

    bool compare_all_as_string(ScamExpr * args)
    {
        MatchMaker<string> reducer(&ScamExpr::toString);
        ExprHandle rv = reducer(args);

        return rv->truth();
    }

    bool compare_all_errors(ScamExpr * args)
    {
        return ( all_same_type(args, &ScamExpr::error) &&
                 compare_all_as_string(args) );
    }

    bool compare_all_booleans(ScamExpr * args)
    {
        return ( all_same_type(args, &ScamExpr::isBoolean) &&
                 compare_all_as_string(args) );
    }

    bool compare_all_chars(ScamExpr * args)
    {
        return ( all_same_type(args, &ScamExpr::isChar) &&
                 compare_all_as_string(args) );
    }

    bool compare_all_strings(ScamExpr * args)
    {
        return ( all_same_type(args, &ScamExpr::isString) &&
                 compare_all_as_string(args) );
    }

    bool compare_all_symbols(ScamExpr * args)
    {
        return ( all_same_type(args, &ScamExpr::isSymbol) &&
                 compare_all_as_string(args) );
    }

    bool compare_all_numeric(ScamExpr * args)
    {
        if ( ! all_same_type(args, &ScamExpr::isNumeric) ) {
            return false;
        }

        MatchMaker<double> reducer(&ScamExpr::toFloat);
        return reducer(args)->truth();
    }

    bool compare_all_nil(ScamExpr * args)
    {
        return all_same_type(args, &ScamExpr::isNil);
    }

    bool compare_cars(ScamExpr * args)
    {
        auto getcars = [] (ScamExpr * arg) -> ExprHandle {
            return arg->nthcar(0);
        };
        ExprHandle cars = apply_map(args, getcars);
        return compare_all(cars.get());
    }

    bool compare_cdrs(ScamExpr * args)
    {
        auto getcdrs = [] (ScamExpr * arg) -> ExprHandle {
            return arg->nthcdr(0);
        };
        ExprHandle cdrs = apply_map(args, getcdrs);
        return compare_all(cdrs.get());
    }

    bool compare_all_cons(ScamExpr * args)
    {
        return ( all_same_type(args, &ScamExpr::isCons)
                 && compare_cars(args)
                 && compare_cdrs(args) );
    }

    bool compare_nth(ScamExpr * args, size_t idx)
    {
        auto getelts = [=] (ScamExpr * arg) -> ExprHandle {
            return arg->nthcar(idx);
        };
        ExprHandle elts = apply_map(args, getelts);
        return compare_all(elts.get());
    }

    bool compare_all_vector(ScamExpr * args)
    {
        if ( ! all_same_type(args, &ScamExpr::isVector) ||
             ! all_same_length(args) ) {
            return false;
        }

        size_t len = args->nthcar(0)->length();
        for ( size_t idx = 0 ; idx < len ; ++idx ) {
            if ( ! compare_nth(args, idx) ) {
                return false;
            }
        }

        return true;
    }

    bool compare_all_identity(ScamExpr * args)
    {
        ScamExpr * first = args->nthcar(0).get();
        size_t len = args->length();

        for ( size_t idx = 1 ; idx < len ; ++idx ) {
            ScamExpr * curr = args->nthcar(idx).get();
            if ( first != curr ) {
                return false;
            }
        }

        return true;
    }

    bool compare_all(ScamExpr * args)
    {
        ExprHandle first = args->nthcar(0);
        if ( first->error() ) {
            return compare_all_errors(args);
        }
        if ( first->isBoolean() ) {
            return compare_all_booleans(args);
        }
        if ( first->isChar() ) {
            return compare_all_chars(args);
        }
        if ( first->isString() ) {
            return compare_all_strings(args);
        }
        if ( first->isSymbol() ) {
            return compare_all_symbols(args);
        }
        if ( first->isNumeric() ) {
            return compare_all_numeric(args);
        }
        if ( first->isNil() ) {
            return compare_all_nil(args);
        }
        if ( first->isCons() ) {
            return compare_all_cons(args);
        }
        if ( first->isVector() ) {
            return compare_all_vector(args);
        }
        if ( first->isProcedure() || first->isClass() || first->isInstance() ) {
            return compare_all_identity(args);
        }


        stringstream s;
        s << "eq?: Unknown type of expression: " << first->toString();
        throw ScamException(s.str());

        return false;
    }

    void equal_p_impl(ScamExpr * args, ContHandle cont)
    {
        if ( zero_args(args, cont) ||
             has_nulls(args, cont) ||
             one_arg(args, cont) ) {
            return;
        }
        if ( compare_all(args) ) {
            cont->run(yes.get());
        }
        else {
            cont->run(no.get());
        }
    }
}
