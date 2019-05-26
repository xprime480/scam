#if ! defined (INSTANTIATOR_HPP)
#define INSTANTIATOR_HPP 1

#include "prim/ValueMapper.hpp"

#include <cstddef>

namespace scam
{
    class SingletonParser;

    class Instantiator : public ValueMapper
    {
    public:
        Instantiator(size_t & counter);

        ScamValue exec(SingletonParser * parser);

    protected:
        ScamValue map_value(ScamValue val);

    private:
        size_t   & counter;
        ScamValue dict;

        ScamValue inst_value(ScamValue expr);
        ScamValue new_mapping(ScamValue expr);
        ScamValue inst_keyword(ScamValue expr);
        ScamValue inst_cons(ScamValue expr);
        ScamValue inst_vector(ScamValue expr);
        ScamValue inst_dict(ScamValue expr);
    };
}

#endif
