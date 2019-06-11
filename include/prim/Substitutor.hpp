#if ! defined (SUBSTITUTOR_HPP)
#define SUBSTITUTOR_HPP 1

#include "prim/ValueMapper.hpp"

namespace scam
{
    class ScamDict;

    class Substitutor : public ValueMapper
    {
    public:
        Substitutor(ScamValue answers);

        ScamValue resolve_value(ScamValue expr);

    protected:
        ScamValue map_value(ScamValue val) override;

    private:
        ScamValue answers;
        ScamValue helper;

        ScamValue resolve_pair(ScamValue expr);
        ScamValue resolve_vector(ScamValue expr);
        bool have_seen(ScamValue expr);
        ScamValue resolve_keyword(ScamValue expr);
        ScamValue resolve_dict(ScamValue expr);
    };
}

#endif
