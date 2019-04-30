#if ! defined (SUBSTITUTOR_HPP)
#define SUBSTITUTOR_HPP 1

#include "prim/ValueMapper.hpp"

namespace scam
{
    class ScamDict;


    class Substitutor : public ValueMapper
    {
    public:
        Substitutor(ScamDict * answers);

        ExprHandle resolve_value(ExprHandle expr);

    protected:
        ExprHandle map_value(ExprHandle val) override;

    private:
        ScamDict * answers;
        ExprHandle helper;

        ExprHandle resolve_cons(ExprHandle expr);
        ExprHandle resolve_vector(ExprHandle expr);
        bool have_seen(ExprHandle expr);
        ExprHandle resolve_keyword(ExprHandle expr);
        ExprHandle resolve_dict(ExprHandle expr);
    };
}

#endif
