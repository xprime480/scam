#if ! defined(MATCH_UNIFY_COMMON_HPP)
#define MATCH_UNIFY_COMMON_HPP 1

namespace scam
{
    class ScamExpr;
    class Continuation;
    class ScamDict;

    class MatchUnifyCommon
    {
    public:
        MatchUnifyCommon(ScamExpr * args, Continuation * cont, bool unify);
        void solve();

    private:
        ScamExpr * args;
        Continuation * cont;
        bool       unify;

        bool checkargs();
        void process();

        ScamExpr *
        check_ignore(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs);

        ScamExpr *
        check_literals(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs);

        ScamExpr *
        check_keyword(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs);

        ScamExpr *
        check_keyword_reversed(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs);

        ScamExpr *
        check_cons(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs);

        ScamExpr *
        check_vector(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs);

        ScamExpr *
        check_dict(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs);

        ScamExpr * exec(ScamDict * dict, ScamExpr * lhs, ScamExpr * rhs);
        ScamExpr * resolve(ScamExpr * expr);
    };
}

#endif
