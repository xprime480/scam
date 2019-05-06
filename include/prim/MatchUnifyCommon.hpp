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

        ExprHandle
        check_ignore(ScamDict * dict, ExprHandle lhs, ExprHandle rhs);

        ExprHandle
        check_literals(ScamDict * dict, ExprHandle lhs, ExprHandle rhs);

        ExprHandle
        check_keyword(ScamDict * dict, ExprHandle lhs, ExprHandle rhs);

        ExprHandle
        check_keyword_reversed(ScamDict * dict, ExprHandle lhs, ExprHandle rhs);

        ExprHandle
        check_cons(ScamDict * dict, ExprHandle lhs, ExprHandle rhs);

        ExprHandle
        check_vector(ScamDict * dict, ExprHandle lhs, ExprHandle rhs);

        ExprHandle
        check_dict(ScamDict * dict, ExprHandle lhs, ExprHandle rhs);

        ExprHandle exec(ScamDict * dict, ExprHandle lhs, ExprHandle rhs);
        ExprHandle resolve(ExprHandle expr);
    };
}

#endif
