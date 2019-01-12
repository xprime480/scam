
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

bool EqualP::equals(ScamExpr const * expr) const
{
    EqualP const * that = dynamic_cast<EqualP const *>(expr);
    return ( that && this->toString() == that->toString() );
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

    bool compare_all(ScamExpr * args)
    {
        ExprHandle first = args->nthcar(0);

        for ( size_t idx = 1 ; idx < args->length() ; ++idx ) {
            ExprHandle op = args->nthcar(idx);
            if ( ! first->equals(op.get()) ) {
                return false;
            }
        }

        return true;
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
