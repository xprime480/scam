#if ! defined(VALUEMAPPER_HPP)
#define VALUEMAPPER_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    class ValueMapper
    {
    protected:
        ExprHandle map_dict(ExprHandle expr);
        ExprHandle map_vector(ExprHandle expr);
        ExprHandle map_cons(ExprHandle expr);
        virtual ExprHandle map_value(ExprHandle expr) = 0;
    };
}

#endif
