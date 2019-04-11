#if ! defined (INSTANTIATOR_HPP)
#define INSTANTIATOR_HPP 1

#include "prim/ValueMapper.hpp"

#include <cstddef>

namespace scam
{
    class ScamDict;
    class ScamExpr;

    class Instantiator : public ValueMapper
    {
    public:
        Instantiator(size_t & counter);

        ScamExpr * exec(ScamExpr * args);

    protected:
        ScamExpr * map_value(ScamExpr * val);

    private:
        size_t   & counter;
        ScamDict * dict;

        ScamExpr * make_error(ScamExpr * args);

        ScamExpr * checkargs(ScamExpr * args, bool & ok);

        ScamExpr * inst_value(ScamExpr * expr);

        ScamExpr * new_mapping(ScamExpr * expr);

        ScamExpr * inst_keyword(ScamExpr * expr);

        ScamExpr * inst_cons(ScamExpr * expr);

        ScamExpr * inst_vector(ScamExpr * expr);

        ScamExpr * inst_dict(ScamExpr * expr);
    };
}

#endif
