#if ! defined(MATCH_UNIFY_COMMON_HPP)
#define MATCH_UNIFY_COMMON_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    class ScamDict;

    class MatchUnifyCommon
    {
    public:
        MatchUnifyCommon(ExprHandle args, Continuation * cont, bool unify);
        void solve();

    private:
        ExprHandle args;
        Continuation * cont;
        bool       unify;

        bool checkargs();
        void process();

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
