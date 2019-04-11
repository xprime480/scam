#if ! defined(VALUEMAPPER_HPP)
#define VALUEMAPPER_HPP 1

namespace scam
{
    class ScamExpr;
  
    class ValueMapper
    {
    protected:

        ScamExpr * map_dict(ScamExpr * expr);

        ScamExpr * map_vector(ScamExpr * expr);

        ScamExpr * map_cons(ScamExpr * expr);

        virtual ScamExpr * map_value(ScamExpr * expr) = 0;
    };
}

#endif
