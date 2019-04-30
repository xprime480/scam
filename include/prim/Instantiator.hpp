#if ! defined (INSTANTIATOR_HPP)
#define INSTANTIATOR_HPP 1

#include "prim/ValueMapper.hpp"

#include <cstddef>

namespace scam
{
    class ScamDict;

    class Instantiator : public ValueMapper
    {
    public:
        Instantiator(size_t & counter);

        ExprHandle exec(ExprHandle args);

    protected:
        ExprHandle map_value(ExprHandle val);

    private:
        size_t   & counter;
        ScamDict * dict;

        ExprHandle make_error(ExprHandle args);
        ExprHandle checkargs(ExprHandle args, bool & ok);
        ExprHandle inst_value(ExprHandle expr);
        ExprHandle new_mapping(ExprHandle expr);
        ExprHandle inst_keyword(ExprHandle expr);
        ExprHandle inst_cons(ExprHandle expr);
        ExprHandle inst_vector(ExprHandle expr);
        ExprHandle inst_dict(ExprHandle expr);
    };
}

#endif
