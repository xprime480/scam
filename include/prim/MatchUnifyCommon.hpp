#if ! defined(MATCH_UNIFY_COMMON_HPP)
#define MATCH_UNIFY_COMMON_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    class MatchUnifyParser;

    class MatchUnifyCommon
    {
    public:
        MatchUnifyCommon(MatchUnifyParser * parser, Continuation * cont);
        void solve();

    private:
        MatchUnifyParser * parser;
        Continuation * cont;

        ScamValue check_ignore(ScamValue dict, ScamValue lhs, ScamValue rhs);
        ScamValue check_literals(ScamValue dict, ScamValue lhs, ScamValue rhs);
        ScamValue check_keyword(ScamValue dict, ScamValue lhs, ScamValue rhs);

        ScamValue
        check_keyword_reversed(ScamValue dict, ScamValue lhs, ScamValue rhs);

        ScamValue check_cons(ScamValue dict, ScamValue lhs, ScamValue rhs);
        ScamValue check_vector(ScamValue dict, ScamValue lhs, ScamValue rhs);
        ScamValue check_dict(ScamValue dict, ScamValue lhs, ScamValue rhs);
        ScamValue exec(ScamValue dict, ScamValue lhs, ScamValue rhs);
        ScamValue resolve(ScamValue expr);
    };
}

#endif
