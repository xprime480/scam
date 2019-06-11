#if ! defined(VALUEMAPPER_HPP)
#define VALUEMAPPER_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    class ValueMapper
    {
    protected:
        ScamValue map_dict(ScamValue expr);
        ScamValue map_vector(ScamValue expr);
        ScamValue map_pair(ScamValue expr);
        virtual ScamValue map_value(ScamValue expr) = 0;
    };
}

#endif
