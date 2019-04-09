
#include "prim/EqualP.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void equal_p_impl(ScamExpr * args, Continuation * cont);
}

EqualP::EqualP()
    : Primitive("eq?")
{
}

EqualP * EqualP::makeInstance()
{
    return new EqualP();
}

void EqualP::applyArgs(ScamExpr * args, Continuation * cont)
{
    equal_p_impl(args, cont);
}

bool EqualP::equals(ScamExpr const * expr) const
{
    EqualP const * that = dynamic_cast<EqualP const *>(expr);
    return ( that && this->toString() == that->toString() );
}

namespace
{
    static ScamExpr * const yes = ExpressionFactory::makeBoolean(true);
    static ScamExpr * const no  = ExpressionFactory::makeBoolean(false);

    extern bool compare_all(ScamExpr * args);

    template <typename MapFn>
    ScamExpr * apply_map(ScamExpr * args, MapFn mapper)
    {
        if ( args->isNil() ) {
            return ExpressionFactory::makeNil();
        }
        if ( ! args->isCons() ) {
            return mapper(args);
        }

        ScamExpr * car = args->nthcar(0);
        ScamExpr * cdr = args->nthcdr(0);

        ScamExpr * newCar = mapper(car);
        ScamExpr * newCdr = apply_map(cdr, mapper);

        return ExpressionFactory::makeCons(newCar, newCdr);
    }

    template <typename MapFn, typename ReduceFn>
    ScamExpr * map_reduce(ScamExpr * args, MapFn mapper, ReduceFn reducer)
    {
        ScamExpr * mapped = apply_map(args, mapper);
        ScamExpr * reduced = reducer(mapped);
        return reduced;
    }

    bool zero_args(ScamExpr * args, Continuation * cont)
    {
        if ( 0 == args->length() ) {
            cont->run(yes);
            return true;
        }
        return false;
    }

#if 0
    ScamExpr * all(ScamExpr * args)
    {
        const size_t len = args->length();
        for ( size_t idx = 0 ; idx < len ; ++idx ) {
            ScamExpr * arg = args->nthcar(idx);
            if ( ! arg->truth() ) {
                return no;
            }
        }

        return yes;
    }
#endif

    ScamExpr * any(ScamExpr * args)
    {
        const size_t len = args->length();
        for ( size_t idx = 0 ; idx < len ; ++idx ) {
            if ( args->nthcar(idx)->truth() ) {
                return yes;
            }
        }
        return no;
    }

    bool has_nulls(ScamExpr * args, Continuation * cont)
    {
        auto fn = [](ScamExpr * arg) -> ScamExpr * {
            return ExpressionFactory::makeBoolean(arg->isNull());
        };
        ScamExpr * answer = map_reduce(args, fn, any);
        return answer->truth();
    }

    bool one_arg(ScamExpr * args, Continuation * cont)
    {
        if ( 1 == args->length() ) {
            cont->run(yes);
            return true;
        }
        return false;
    }

    bool compare_all(ScamExpr * args)
    {
        ScamExpr * first = args->nthcar(0);

        for ( size_t idx = 1 ; idx < args->length() ; ++idx ) {
            ScamExpr * op = args->nthcar(idx);
            if ( ! first->equals(op) ) {
                return false;
            }
        }

        return true;
    }

    void equal_p_impl(ScamExpr * args, Continuation * cont)
    {
        if ( zero_args(args, cont) ||
             has_nulls(args, cont) ||
             one_arg(args, cont) ) {
            return;
        }
        if ( compare_all(args) ) {
            cont->run(yes);
        }
        else {
            cont->run(no);
        }
    }
}
