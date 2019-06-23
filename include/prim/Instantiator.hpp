#if ! defined (INSTANTIATOR_HPP)
#define INSTANTIATOR_HPP 1

#include "prim/ValueMapper.hpp"

#include <cstddef>

namespace scam
{
    class Instantiator : public ValueMapper
    {
    public:
        Instantiator(size_t & counter);

        ScamValue exec(ScamValue value);

    protected:
        ScamValue map_value(ScamValue val);

    private:
        size_t   & counter;
        ScamValue dict;

        ScamValue inst_value(ScamValue expr);
        ScamValue new_mapping(ScamValue expr);
        ScamValue inst_keyword(ScamValue expr);
        ScamValue inst_pair(ScamValue expr);
        ScamValue inst_vector(ScamValue expr);
        ScamValue inst_dict(ScamValue expr);
    };
}

#endif
