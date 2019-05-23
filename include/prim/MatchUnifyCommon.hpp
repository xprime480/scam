#if ! defined(MATCH_UNIFY_COMMON_HPP)
#define MATCH_UNIFY_COMMON_HPP 1

#include "ScamFwd.hpp"
#include "expr/ExprFwd.hpp"

namespace scam
{
    class MatchUnifyParser;
    class ScamDict;

    class MatchUnifyCommon
    {
    public:
        MatchUnifyCommon(MatchUnifyParser * parser, Continuation * cont);
        void solve();

    private:
        MatchUnifyParser * parser;
        Continuation * cont;

        ScamValue
        check_ignore(ScamDict * dict, ScamValue lhs, ScamValue rhs);

        ScamValue
        check_literals(ScamDict * dict, ScamValue lhs, ScamValue rhs);

        ScamValue
        check_keyword(ScamDict * dict, ScamValue lhs, ScamValue rhs);

        ScamValue
        check_keyword_reversed(ScamDict * dict, ScamValue lhs, ScamValue rhs);

        ScamValue
        check_cons(ScamDict * dict, ScamValue lhs, ScamValue rhs);

        ScamValue
        check_vector(ScamDict * dict, ScamValue lhs, ScamValue rhs);

        ScamValue
        check_dict(ScamDict * dict, ScamValue lhs, ScamValue rhs);

        ScamValue exec(ScamDict * dict, ScamValue lhs, ScamValue rhs);
        ScamValue resolve(ScamValue expr);
    };
}

#endif
