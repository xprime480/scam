#if ! defined (SUBSTITUTOR_HPP)
#define SUBSTITUTOR_HPP 1

#include "prim/ValueMapper.hpp"

namespace scam
{
    class ScamDict;
    class ScamExpr;

    class Substitutor : public ValueMapper
    {
    public:
        Substitutor(ScamDict * answers);

        ScamExpr * resolve_value(ScamExpr * expr);

    protected:
        ScamExpr * map_value(ScamExpr * val) override;

    private:
        ScamDict * answers;
        ScamExpr * helper;

        ScamExpr * resolve_cons(ScamExpr * expr);

        ScamExpr * resolve_vector(ScamExpr * expr);

        bool have_seen(ScamExpr * expr);

        ScamExpr * resolve_keyword(ScamExpr * expr);

        ScamExpr * resolve_dict(ScamExpr * expr);
    };
}

#endif
