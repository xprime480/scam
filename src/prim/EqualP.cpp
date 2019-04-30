#include "prim/EqualP.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void equal_p_impl(ExprHandle args, Continuation * cont);
}

EqualP::EqualP()
    : Primitive("eq?")
{
}

EqualP * EqualP::makeInstance()
{
    return new EqualP();
}

void EqualP::applyArgs(ExprHandle args, Continuation * cont)
{
    equal_p_impl(args, cont);
}

bool EqualP::equals(ConstExprHandle expr) const
{
    EqualP const * that = dynamic_cast<EqualP const *>(expr);
    return ( that && this->toString() == that->toString() );
}

namespace
{
    static ExprHandle const yes = ExpressionFactory::makeBoolean(true);
    static ExprHandle const no  = ExpressionFactory::makeBoolean(false);

    extern bool compare_all(ExprHandle args);

    template <typename MapFn>
    ExprHandle apply_map(ExprHandle args, MapFn mapper)
    {
        if ( args->isNil() ) {
            return ExpressionFactory::makeNil();
        }
        if ( ! args->isCons() ) {
            return mapper(args);
        }

        ExprHandle car = args->nthcar(0);
        ExprHandle cdr = args->nthcdr(0);

        ExprHandle newCar = mapper(car);
        ExprHandle newCdr = apply_map(cdr, mapper);

        return ExpressionFactory::makeCons(newCar, newCdr);
    }

    template <typename MapFn, typename ReduceFn>
    ExprHandle map_reduce(ExprHandle args, MapFn mapper, ReduceFn reducer)
    {
        ExprHandle mapped = apply_map(args, mapper);
        ExprHandle reduced = reducer(mapped);
        return reduced;
    }

    bool zero_args(ExprHandle args, Continuation * cont)
    {
        if ( 0 == args->length() ) {
            cont->run(yes);
            return true;
        }
        return false;
    }

#if 0
    ExprHandle all(ExprHandle args)
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
#endif

    ExprHandle any(ExprHandle args)
    {
        const size_t len = args->length();
        for ( size_t idx = 0 ; idx < len ; ++idx ) {
            if ( args->nthcar(idx)->truth() ) {
                return yes;
            }
        }
        return no;
    }

    bool has_nulls(ExprHandle args, Continuation * cont)
    {
        auto fn = [](ExprHandle arg) -> ExprHandle {
            return ExpressionFactory::makeBoolean(arg->isNull());
        };
        ExprHandle answer = map_reduce(args, fn, any);
        return answer->truth();
    }

    bool one_arg(ExprHandle args, Continuation * cont)
    {
        if ( 1 == args->length() ) {
            cont->run(yes);
            return true;
        }
        return false;
    }

    bool compare_all(ExprHandle args)
    {
        ExprHandle first = args->nthcar(0);

        for ( size_t idx = 1 ; idx < args->length() ; ++idx ) {
            ExprHandle op = args->nthcar(idx);
            if ( ! first->equals(op) ) {
                return false;
            }
        }

        return true;
    }

    void equal_p_impl(ExprHandle args, Continuation * cont)
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
